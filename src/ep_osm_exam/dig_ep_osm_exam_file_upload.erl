
-module(dig_ep_osm_exam_file_upload).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

-define(AWS_S3_DEFAULT_REGION, "ap-south-1").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------


upload(ObjectKey) ->
	%
	% upload in background as it is time consuming
	%
	Context = wf_context:context(),
	PId = spawn(?MODULE, upload1, [Context, ObjectKey]),
	dig:log(io_lib:format("spawned upload process: ~p", [PId])).


upload1(Context, ObjectKey) ->

	try

		%
		% init
		%
		wf_context:context(Context),


		%
		% download from s3
		%
		Filepath = handle_download_from_s3(ObjectKey),


		%
		% verify file is zip
		%
		handle_verify_zip_file(Filepath),


		%
		% create a working directory and unzip the zip file
		%
		WorkDir = "/tmp/" ++ helper:uidintstr(),
		helper:cmd("mkdir -p ~s", [WorkDir]),


		%
		% upload to exam
		%
		handle_upload_to_exam(ObjectKey, WorkDir, Filepath),


		%
		% cleanup
		%
		handle_cleanup(WorkDir, Filepath),
		handle_remove_from_s3(ObjectKey),


		%
		% done
		%
		dig:log("Processing done.")

	catch
		Error:Message ->
			?D({Error, Message, erlang:get_stacktrace()}),
			Log = io_lib:format("~p, ~p", [Error, Message]),
			dig:log(error, Log)
	end.



%------------------------------------------------------------------------------
% handle - verify zip file
%------------------------------------------------------------------------------

handle_verify_zip_file(Filepath) ->

	%
	% init
	%
	dig:log("Verifying file is zip file"),


	%
	% find file type
	%
	CmdRes = helper:cmd("file -b ~s", [Filepath]),
	Type = lists:nth(1, string:tokens(CmdRes, " ")),


	%
	% assert - file type should be zip
	%
	?ASSERT(
		string:to_upper(Type) == "ZIP",
		"ERROR: uploaded file is not a zip file!"
	),


	%
	% ok
	%
	dig:log("Verifying file is zip file ... ok").



%------------------------------------------------------------------------------
% handle - upload to exam
%------------------------------------------------------------------------------

handle_upload_to_exam(ObjectKey, WorkDir, Filepath) ->


	%
	% init
	%
	dig:log("Uploading to exam"),

	[SeasonId, _] = string:tokens(ObjectKey, "_"),


	%
	% unzip the file in workdir
	%
	helper:cmd("cd ~s; unzip ~s", [WorkDir, Filepath]),


	%
	% find the zip dir
	%
	{ok, [ZipDir0 | _]} = file:list_dir(WorkDir),
	ZipDir = string:join([WorkDir, ZipDir0], "/"),

	%
	% proces dirs under zip dir one by one
	%
	{ok, DirsInZipDir} = file:list_dir(ZipDir),
	lists:foreach(fun(Dir) ->

		%
		% init
		%
		dig:log(info, ?FLATTEN(io_lib:format("Processing directory ~s/~s", [ZipDir, Dir]))),


		%
		% find test with matching season and dir as course id
		%
		Fs = [
			itf:build(?COREXS(season_fk), SeasonId),
			fields:build(anptestcourseid, Dir)
		],
		#db2_find_response {docs=ExamDocs} = db2_find:get_by_fs(
			anptests:getdb(), Fs, 0, ?INFINITY
		),


		%
		% if found, upload files
		%
		case ExamDocs of
			[] ->
				dig:log(error, "SKIPPED. Could not find test for " ++ Dir);
			[ExamDoc] ->
				handle_upload_files_to_exam(ExamDoc, string:join([ZipDir, Dir], "/"));
			_ ->
				dig:log(error, "SKIPPED. Multiple tests found for  " ++ Dir)
		end


	end, DirsInZipDir),



	dig:log("Uploading to exam ... ok").



%------------------------------------------------------------------------------
% handle - upload files to exam
%------------------------------------------------------------------------------

handle_upload_files_to_exam(ExamDoc, Dir) ->

	%
	% list files with sizes
	%
	{ok, Files} = file:list_dir(Dir),


	%
	% get only pdf and erl files
	%
	FilesPdfOrErl = lists:filter(fun(File) ->
		case string:to_lower(filename:extension(File)) of
			".pdf" ->
				true;
			".erl" ->
				true;
			_ ->
				false
		end
	end, Files),


	FilesWithSize = lists:map(fun(File) ->
		{File, filelib:file_size(string:join([Dir, File], "/"))}
	end, FilesPdfOrErl),


	%
	% sort pdf files by size (smallest first)
	%
	FilesWithSizeSorted = lists:sort(fun({_, A}, {_, B}) ->
		A < B
	end, FilesWithSize),



	%
	% uplaod pdf files
	%
	FilesPdf = lists:filter(fun({File, _}) ->
		string:to_lower(filename:extension(File)) == ".pdf"
	end, FilesWithSizeSorted),
	case FilesPdf of
		[] ->
			dig:log(error, "Question paper: NOT FOUND!"),
			dig:log(error, "Model answer: NOT FOUND!");
		[QuestionPaper] ->
			handle_upoad_file(ExamDoc, Dir, QuestionPaper, "questionpaper.pdf"),
			dig:log(success, io_lib:format("Question paper: ~p", [QuestionPaper])),
			dig:log(error, "Model answer: NOT FOUND!");
		[QuestionPaper, ModelAnswer | _] ->
			handle_upoad_file(ExamDoc, Dir, QuestionPaper, "questionpaper.pdf"),
			dig:log(success, io_lib:format("Question paper: ~p", [QuestionPaper])),
			handle_upoad_file(ExamDoc, Dir, ModelAnswer, "modelanswers.pdf"),
			dig:log(success, io_lib:format("Model answer: ~p", [ModelAnswer]))
	end,


	%
	% file erl file
	%
	FilesErl = lists:filter(fun({File, _}) ->
		string:to_lower(filename:extension(File)) == ".erl"
	end, FilesWithSizeSorted),
	case FilesErl of
		[] ->
			dig:log(warning, "Marking file: NOT FOUND!");
		[MarkingFile | _] ->
			handle_upoad_file(ExamDoc, Dir, MarkingFile, "marking.erl"),
			dig:log(success, io_lib:format("Marking file: ~p", [MarkingFile]))
	end.




%------------------------------------------------------------------------------
% handle - upload file
%------------------------------------------------------------------------------

handle_upoad_file(ExamDoc0, Dir, {Filename, _}, Suffix) ->

	%
	% init
	%
	{ok, ExamDoc} = anptests:getdoc(itf:idval(ExamDoc0)),
	AttachmentName = ?FLATTEN(io_lib:format("~s_~s", [
		itf:val(ExamDoc, anptestcourseid),
		Suffix
	])),
	AttachmentLocation = string:join([Dir, Filename], "/"),

	%
	% upload
	%
	{ok, attachment_uploaded} = attachment:upload(
		anptests:getdb(),
		itf:idval(ExamDoc),
		itf:revval(ExamDoc),
		AttachmentName,
		AttachmentLocation
	).



%------------------------------------------------------------------------------
% handle - upload to s3: check dir exists
%------------------------------------------------------------------------------

handle_upload_to_s3_check_directory_exists(ZipDir, DirNameToUpload) ->
	Dir = ?FLATTEN(ZipDir ++ "/" ++ DirNameToUpload),
	filelib:is_dir(Dir).


%------------------------------------------------------------------------------
% handle - upload to s3: upload
%------------------------------------------------------------------------------

handle_upload_to_s3_upload(ZipDir, S3Dir, DirNameToUpload) ->

	%
	% exec
	%
	CmdRes = helper:cmd("cd ~s; AWS_ACCESS_KEY_ID=~s AWS_SECRET_ACCESS_KEY=~s AWS_DEFAULT_REGION=~s aws s3 sync --only-show-errors ~s s3://~s/~s/~s", [
		ZipDir,
		configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region, ?AWS_S3_DEFAULT_REGION),
		DirNameToUpload, helper_s3:aws_s3_bucket(), S3Dir, DirNameToUpload
	]),
	CmdRes.



%------------------------------------------------------------------------------
% handle - cleanup
%------------------------------------------------------------------------------

handle_cleanup(WorkDir, Filepath) ->

	dig:log("Cleaning up"),

	%
	% remove uploaded file
	%
	[] = helper:cmd("rm -rf ~s", [WorkDir]),
	[] = helper:cmd("rm -f ~s", [Filepath]),


	dig:log("Cleaning up ... ok").



%------------------------------------------------------------------------------
% handle - download from s3
%------------------------------------------------------------------------------

handle_download_from_s3(ObjectKey) ->

	dig:log("Downloading zip from S3."),


	{ok, Cwd} = file:get_cwd(),
	Fileloc = ?FLATTEN(io_lib:format("~s/scratch/~s", [Cwd, ObjectKey])),

	CmdRes = helper:cmd("AWS_ACCESS_KEY_ID=~s AWS_SECRET_ACCESS_KEY=~s AWS_DEFAULT_REGION=~s aws s3 cp --only-show-errors s3://~s/~s/~s ~s", [
		configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region, ?AWS_S3_DEFAULT_REGION),
		helper_s3:aws_s3_bucket(), "browser_to_s3", ObjectKey,
		Fileloc
	]),

	?ASSERT(
		CmdRes == [],
		?FLATTEN(io_lib:format("ERROR: download from s3 failed! ~p", [CmdRes]))
	),


	dig:log("Downloading zip from S3 ... ok"),

	Fileloc.



%------------------------------------------------------------------------------
% handle - remove from s3
%------------------------------------------------------------------------------

handle_remove_from_s3(ObjectKey) ->

	CmdRes = helper:cmd("AWS_ACCESS_KEY_ID=~s AWS_SECRET_ACCESS_KEY=~s AWS_DEFAULT_REGION=~s aws s3 rm --only-show-errors s3://~s/~s/~s", [
		configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region, ?AWS_S3_DEFAULT_REGION),
		helper_s3:aws_s3_bucket(), "browser_to_s3", ObjectKey
	]),

	?ASSERT(
		CmdRes == [],
		"ERROR: remove from s3 failed!"
	),

	CmdRes.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
