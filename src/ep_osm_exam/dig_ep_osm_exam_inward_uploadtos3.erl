
-module(dig_ep_osm_exam_inward_uploadtos3).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------


upload(S3Dir, DirNamesToUpload, Filename, Filepath) ->
	%
	% upload in background as it is time consuming
	%
	Context = wf_context:context(),
	PId = spawn(?MODULE, upload1, [Context, S3Dir, DirNamesToUpload, Filename, Filepath]),
	dig:log(io_lib:format("spawned upload process: ~p", [PId])).


upload1(Context, S3Dir, DirNamesToUpload, Filename, Filepath0) ->

	try

		%
		% init
		%
		wf_context:context(Context),


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
		% verify inputs
		%
		handle_verify_inputs(S3Dir, DirNamesToUpload),


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
		% upload to s3
		%
		handle_upload_to_s3(WorkDir, S3Dir, DirNamesToUpload, Filename, Filepath),


		%
		% cleanup
		%
		handle_cleanup(WorkDir, Filepath),
		handle_remove_from_s3(Filename),


		%
		% done
		%
		dig:log("Uploading done.")

	catch
		Error:Message ->
			?D({Error, Message, erlang:get_stacktrace()}),
			Log = io_lib:format("~p, ~p", [Error, Message]),
			dig:log(error, Log)
	end.



%------------------------------------------------------------------------------
% handle - verify inputs
%------------------------------------------------------------------------------

handle_verify_inputs(S3Dir, DirNamesToUpload) ->

	%
	% init
	%
	dig:log("Verifying inputs"),


	?ASSERT(
		configs:get(aws_s3_bucket, []) /= [],
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

handle_upload_to_s3(WorkDir, S3Dir, DirNamesToUpload, _Filename, Filepath) ->


	%
	% init
	%
	dig:log("Uploading to S3"),


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
	% directories as serial numbers as prefix, remove the prefix
	%
	handle_remove_prefix(ZipDir),



	%
	% for each directory, check if exists and upload to s3
	%
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
	CmdRes = helper:cmd("cd ~s; AWS_ACCESS_KEY_ID=~s AWS_SECRET_ACCESS_KEY=~s AWS_DEFAULT_REGION=~s aws s3 sync --only-show-errors ~s s3://~s/~s/~s", [
		ZipDir,
		configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region),
		DirNameToUpload, configs:get(aws_s3_bucket), S3Dir, DirNameToUpload
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
		configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region),
		configs:get(aws_s3_bucket), "browser_to_s3", ObjectKey,
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
		configs:get(aws_s3_bucket), "browser_to_s3", ObjectKey
	]),

	?ASSERT(
		CmdRes == [],
		"ERROR: remove from s3 failed!"
	),

	CmdRes.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
