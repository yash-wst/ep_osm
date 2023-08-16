
-module(dig_ep_osm_exam_inward_uploadtos3).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------


upload(S3Dir, DirNamesToUpload, Filename, Filepath, BundleId) ->
	%
	% upload in background as it is time consuming
	%
	Context = wf_context:context(),
	PId = spawn(?MODULE, upload1, [
		Context, S3Dir, DirNamesToUpload, Filename, Filepath, BundleId
	]),
	dig:log(io_lib:format("spawned upload process: ~p", [PId])).


upload1(Context, S3Dir, DirNamesToUpload, Filename, Filepath0, BundleId) ->
	try
		upload2(Context, S3Dir, DirNamesToUpload, Filename, Filepath0, BundleId)
	catch
		Error:Message ->
			?D({Error, Message, erlang:get_stacktrace()}),
			Log = io_lib:format("~p, ~p", [Error, Message]),
			dig:log(error, Log)
	end.


upload2(Context, S3Dir, DirNamesToUpload, Filename, Filepath0, BundleId) ->
	%
	% init
	%
	case Context of
		undefined ->
			skip;
		_ ->
			wf_context:context(Context)
	end,


	%
	% download from s3
	%
	Filepath = case Filepath0 of
		undefined ->
			handle_download_from_s3(Filename);
		_ ->
			Filepath0
	end,


	%
	% create a working directory and unzip the zip file
	%
	WorkDir = itx:format("~s/~s", [get_workdir_root(), helper:uidintstr()]),
	helper:cmd("mkdir -p ~s", [WorkDir]),


	%
	% upload
	%
	try
		upload3(S3Dir, DirNamesToUpload, Filename, Filepath, BundleId, WorkDir)
	catch Error:Message ->
		handle_cleanup(WorkDir, Filepath),
		handle_remove_from_s3(Filename),
		throw({Error, Message})
	end.



upload3(S3Dir, DirNamesToUpload, Filename, Filepath, BundleId, WorkDir) ->
	%
	% verify inputs
	%
	handle_verify_inputs(S3Dir, DirNamesToUpload),


	%
	% verify file is zip
	%
	handle_verify_zip_file(Filepath),



	%
	% upload to s3
	%
	handle_upload_to_s3(WorkDir, S3Dir, DirNamesToUpload, Filename, Filepath, BundleId),


	%
	% cleanup
	%
	handle_cleanup(WorkDir, Filepath),
	handle_remove_from_s3(Filename),


	%
	% done
	%
	dig:log("Uploading done.").




%------------------------------------------------------------------------------
% handle - verify inputs
%------------------------------------------------------------------------------

handle_verify_inputs(S3Dir, DirNamesToUpload) ->

	%
	% init
	%
	dig:log("Verifying inputs"),


	?ASSERT(
		helper_s3:aws_s3_bucket() /= [],
		"ERROR: aws_s3_bucket not set"
	),

	?ASSERT(
		configs:get(aws_s3_access_key, []) /= [],
		"ERROR: aws_s3_access_key not set"
	),


	?ASSERT(
		configs:get(aws_s3_secret, []) /= [],
		"ERROR: aws_s3_secret not set"
	),

	?ASSERT(
		configs:get(aws_s3_default_region, []) /= [],
		"ERROR: aws_s3_default_region not set"
	),


	?ASSERT(
		S3Dir /= [],
		"ERROR: S3 directory not set!"
	),



	?ASSERT(
		DirNamesToUpload /= [],
		"ERROR: nothing to upload!"
	),



	%
	% ok
	%
	dig:log("Verifying inputs ... ok").





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
% handle - upload to s3
%------------------------------------------------------------------------------

handle_upload_to_s3(WorkDir, S3Dir, DirNamesToUpload, _Filename, Filepath, BundleId) ->


	%
	% init
	%
	dig:log("Unzipping file"),
	{ok, BundleDoc} = ep_osm_bundle_api:get(BundleId),
	ExamId = itf:val(BundleDoc, osm_exam_fk),
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),


	%
	% unzip the file in workdir
	%
	helper:cmd("cd ~s; unzip ~s", [WorkDir, Filepath]),


	%
	% find the zip dir
	%
	{ok, [ZipDir0]} = file:list_dir(WorkDir),
	ZipDir = io_lib:format("~s/~s", [WorkDir, ZipDir0]),


	%
	% handle verify uploaded folder name
	%
	dig:log("Verifying folder name"),
	handle_verify_uploaded_folder_name(ZipDir0, ExamDoc, BundleDoc),



	%
	% directories as serial numbers as prefix, remove the prefix
	%
	handle_remove_prefix(ZipDir),



	%
	% detect bad images
	%
	BadFiles = handle_detect_bad_images(
		WorkDir, ZipDir0, DirNamesToUpload,
		itxconfigs_cache:get2(ep_osm_detect_bad_images, false)
	),



	%
	% record bad files in candidate docs
	%
	handle_record_bad_images_in_candidate_docs(
		itxconfigs_cache:get2(ep_osm_detect_bad_images, false),
		BadFiles, ExamId
	),



	%
	% for each directory, check if exists and upload to s3
	%
	dig:log("Uploading to S3"),
	lists:foreach(fun(DirNameToUpload) ->
		case handle_upload_to_s3_check_directory_exists(ZipDir, DirNameToUpload) of
			true ->
				UploadRes = handle_upload_to_s3_upload(
					ZipDir, S3Dir, DirNameToUpload
				),
				Log = io_lib:format("Processing ... ~s: ~s", [
					DirNameToUpload, UploadRes
				]),
				dig:log(Log);
			_ ->
				Log = io_lib:format("Processing ... ~s: skipped! directory not found in zip", [
					DirNameToUpload
				]),
				dig:log(Log)
		end
	end, DirNamesToUpload),


	dig:log("Uploading to S3 ... ok").



%------------------------------------------------------------------------------
% handle - detect bad images
%------------------------------------------------------------------------------

handle_detect_bad_images(WorkDir, ZipDir, DirNamesToUpload, _DetectBadImages = true) ->


	dig:log("Detecting bad images ..."),


	%
	% create a file with list of all image paths
	%
	Filepaths = handle_detect_bad_images_get_filepaths(WorkDir, ZipDir, DirNamesToUpload),
	FilepathsFile = itx:format("~s/filepaths.txt", [WorkDir]),
	ok = file:write_file(FilepathsFile, string:join(Filepaths, "\n")),
	Outfile = itx:format("~s/filepaths.out", [WorkDir]),
	

	%
	% run bad image detection model and create output file
	%
	helper:cmd("classify -f ~s -o ~s", [
		FilepathsFile, Outfile
	]),



	%
	% parse output file
	%
	BadFiles = handle_detect_bad_images_parse_output(Outfile),
	BadFiles1 = string:join(BadFiles, " "),
	dig:log(error, itx:format("~s", [BadFiles1])),


	dig:log("Detecting bad images ... end."),
	BadFiles;



handle_detect_bad_images(_WorkDir, _ZipDir, _DirNamesToUpload, _DetectBadImages = false) ->
	dig:log("Detecting bad images ... skipped.").




handle_detect_bad_images_parse_output(Outfile) ->

	{ok, FileBin} = file:read_file(Outfile),
	FileRes = string:tokens(?B2L(FileBin), "\n"),



	%
	% identify bad files
	%
	lists:foldl(fun(Line, Acc) ->

		%
		% get the image goodness value
		%
		Line1 = helper:trim(Line, "\""),
		Vals = jsx:decode(?L2B(Line1)),
		BadVal = proplists:get_value(<<"bad">>, Vals, 0),

		case BadVal > 0.5 of
			true ->
				Path = proplists:get_value(<<"Image">>, Vals),
				PathTokens = string:tokens(?B2L(Path), "/"),
				PathLen = length(PathTokens),
				PathTokens1 = lists:sublist(PathTokens, PathLen - 1, PathLen),
				PathX = string:join(PathTokens1, "/"),
				Acc ++ [PathX];
			_ ->
				Acc
		end

	end, [], FileRes).





%
% handle_detect_bad_images_get_filepaths
%
handle_detect_bad_images_get_filepaths(WorkDir, ZipDir, DirNamesToUpload) ->

	lists:foldl(fun(Dir, Acc) ->

		%
		% get all files in dir
		%
		DirFullPath = itx:format("~s/~s/~s", [WorkDir, ZipDir, Dir]),
		{ok, Files} = file:list_dir(DirFullPath),
		

		%
		% filter our jpg files
		%
		Files1 = lists:filter(fun(File) ->
			string:to_lower(filename:extension(File)) == ".jpg"
		end, Files),



		%
		% get full paths
		%
		Filepaths = lists:map(fun(File) ->
			itx:format("~s/~s", [DirFullPath, File])
		end, Files1),



		%
		% return
		%
		Acc ++ Filepaths


	end, [], DirNamesToUpload).



%------------------------------------------------------------------------------
% handle - record bad images in candidate docs
%------------------------------------------------------------------------------

handle_record_bad_images_in_candidate_docs(_DetectBadImages = true, BadFiles, ExamId) ->

	%
	% init
	%
	dig:log("Updating bad images list in candidate doc ..."),
	ExamDb = anpcandidates:db(ExamId),


	%
	% create dict
	%
	Dict = lists:foldl(fun(BadFile, AccDict) ->
		[SeatNumber, Filename] = string:tokens(BadFile, "/"),
		dict:append(SeatNumber, Filename, AccDict)
	end, dict:new(), BadFiles),


	%
	% get candidate docs
	%
	SeatNumbers = dict:fetch_keys(Dict),
	Docs = anpcandidates:getdocs_by_snos(ExamId, SeatNumbers),


	%
	% save bad images list
	%
	ListOfFsDoc = lists:map(fun(CDoc) ->
		%
		% init
		%
		FsDoc = helper_api:doc2fields({ok, CDoc}),
		SeatNumber = itf:val(CDoc, anpseatnumber),
		CandidateBadFiles = case dict:find(SeatNumber, Dict) of
			{ok, List} ->
				List;
			_ ->
				[]
		end,


		%
		% merge fs
		%
		itf:fs_merge(FsDoc, [
			fields:build(autoqc_images, CandidateBadFiles)
		])


	end, Docs),
	{ok, _} = anpcandidates:savebulk(ExamDb, ListOfFsDoc),
	dig:log("Updating bad images list in candidate doc ... ok");

handle_record_bad_images_in_candidate_docs(_, _, _) ->
	ok.


%------------------------------------------------------------------------------
% handle - verify uploaded folder name
%------------------------------------------------------------------------------

handle_verify_uploaded_folder_name(ZipDir0, ExamDoc, BundleDoc) ->
	BundleDirName = dig_ep_osm_exam_inward:get_bundle_dir_name(ExamDoc, BundleDoc),
	?ASSERT(
		ZipDir0 == BundleDirName,
		itx:format("Incorrect folder uploaded. Required: ~s, uploaded: ~s", [
			BundleDirName, ZipDir0
		])
	).



%------------------------------------------------------------------------------
% handle - remove prefix
%------------------------------------------------------------------------------

handle_remove_prefix(ZipDir) ->

	{ok, Dirs} = file:list_dir(ZipDir),
	lists:foreach(fun(Dir) ->

		%
		% remove prefix
		%
		Dir1 = case string:tokens(Dir, ".") of
			[_H | []] ->
				Dir;
			[_H | Tail] ->
				Tail
		end,

		%
		% rename dir
		%
		Source = ?FLATTEN(io_lib:format("~s/~s", [ZipDir, Dir])),
		Destination = ?FLATTEN(io_lib:format("~s/~s", [ZipDir, Dir1])),
		ok = file:rename(Source, Destination)

	end, Dirs).



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
	CmdRes = helper:cmd("cd ~s; AWS_ACCESS_KEY_ID=~s AWS_SECRET_ACCESS_KEY=~s AWS_DEFAULT_REGION=~s aws s3 sync --only-show-errors ~s s3://~s/~s/~s --delete", [
		ZipDir,
		configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region),
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


	Fileloc = ?FLATTEN(io_lib:format("~s/~s", [get_workdir_root(), ObjectKey])),

	CmdRes = helper:cmd("AWS_ACCESS_KEY_ID=~s AWS_SECRET_ACCESS_KEY=~s AWS_DEFAULT_REGION=~s aws s3 cp --only-show-errors s3://~s/~s/~s ~s", [
		configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region),
		helper_s3:aws_s3_bucket(), "browser_to_s3", ObjectKey,
		Fileloc
	]),

	?ASSERT(
		CmdRes == [],
		"ERROR: download from s3 failed!"
	),


	dig:log("Downloading zip from S3 ... ok"),

	Fileloc.



%------------------------------------------------------------------------------
% handle - remove from s3
%------------------------------------------------------------------------------

handle_remove_from_s3(ObjectKey) ->

	CmdRes = helper:cmd("AWS_ACCESS_KEY_ID=~s AWS_SECRET_ACCESS_KEY=~s AWS_DEFAULT_REGION=~s aws s3 rm --only-show-errors s3://~s/~s/~s", [
		configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region),
		helper_s3:aws_s3_bucket(), "browser_to_s3", ObjectKey
	]),

	?ASSERT(
		CmdRes == [],
		"ERROR: remove from s3 failed!"
	),

	CmdRes.





%------------------------------------------------------------------------------
% handle - upload seatnumber zip
%------------------------------------------------------------------------------

handle_upload_seatnumber_zip(ExamId,  CandidateId, SeatNumber, AttachmentName, Filepath) ->
	handle_upload_seatnumber_zip_verify(SeatNumber, AttachmentName, Filepath),
	handle_upload_seatnumber_zip_upload_dir(ExamId, CandidateId, SeatNumber, AttachmentName, Filepath).


%..............................................................................
%
% handle - seatnumber zip verify
%
%..............................................................................

handle_upload_seatnumber_zip_verify(SeatNumber, AttachmentName, Filepath) ->

	%
	% init
	%
	ExpectedFilename = itx:format("~s.zip", [SeatNumber]),

	%
	% assert filename is seatnumber.zip
	%
	?ASSERT(
		AttachmentName == ExpectedFilename,
		itx:format("Invalid file name. Please uploaded ~s", [ExpectedFilename])
	),


	%
	% assert file type is zip
	%
	handle_verify_zip_file(Filepath).




%..............................................................................
%
% handle - seatnumber zip upload
%
%..............................................................................

handle_upload_seatnumber_zip_upload_dir(ExamId, _CandidateId, SeatNumber, _Filename, Filepath) ->

	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),
	S3Dir = itf:val(ExamDoc, aws_s3_dir),



	%
	% create a working directory and unzip the zip file
	%
	WorkDir = "/tmp/" ++ helper:uidintstr(),
	helper:cmd("mkdir -p ~s", [WorkDir]),



	%
	% unzip the file in workdir
	%
	{ok, Cwd} = file:get_cwd(),
	Filepath1 = ?FLATTEN(io_lib:format("~s/~s", [Cwd, Filepath])),
	helper:cmd("cd ~s; unzip ~s", [WorkDir, Filepath1]),



	%
	% upload
	%
	UploadRes = handle_upload_to_s3_upload(
		WorkDir, S3Dir, SeatNumber
	),



	%
	% cleanup
	%
	handle_cleanup(WorkDir, Filepath),



	%
	% assert uploadres is empty
	%
	?ASSERT(
		UploadRes == [],
		UploadRes
	).




get_workdir_root() ->
	case itxconfigs_cache:get2(ep_osm_exam_inward_upload_workdir, "tmp") of
		"scratch" ->
			{ok, Cwd} = file:get_cwd(),
			itx:format("~s/scratch/upload_to_s3", [Cwd]);
		_ ->
			"/tmp/upload_to_s3"
	end.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
