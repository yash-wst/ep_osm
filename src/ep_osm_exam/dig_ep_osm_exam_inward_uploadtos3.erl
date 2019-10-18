
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


upload1(Context, S3Dir, DirNamesToUpload, Filename, Filepath) ->

	try

		%
		% init
		%
		wf_context:context(Context),


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
		"error: aws_s3_bucket not set"
	),

	?ASSERT(
		configs:get(aws_s3_access_key, []) /= [],
		"error: aws_s3_access_key not set"
	),


	?ASSERT(
		configs:get(aws_s3_secret, []) /= [],
		"error: aws_s3_secret not set"
	),

	?ASSERT(
		configs:get(aws_s3_default_region, []) /= [],
		"error: aws_s3_default_region not set"
	),


	?ASSERT(
		S3Dir /= [],
		"error: S3 directory not set!"
	),



	?ASSERT(
		DirNamesToUpload /= [],
		"error: nothing to upload!"
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
		"error: uploaded file is not a zip file!"
	),


	%
	% ok
	%
	dig:log("Verifying file is zip file ... ok").



%------------------------------------------------------------------------------
% handle - upload to s3
%------------------------------------------------------------------------------

handle_upload_to_s3(WorkDir, S3Dir, DirNamesToUpload, Filename, Filepath) ->


	%
	% init
	%
	dig:log("Uploading to S3"),
	ZipDir = io_lib:format("~s/~s", [WorkDir, filename:rootname(Filename)]),



	%
	% unzip the file in workdir
	%
	{ok, Pwd} = file:get_cwd(),
	helper:cmd("cd ~s; unzip ~s/~s", [WorkDir, Pwd, Filepath]),


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
% end
%------------------------------------------------------------------------------
