
-module(dig_ep_osm_exam_inward_uploadtos3).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------


upload(S3Dir, DirNamesToUpload, Filename, Filepath) ->

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
	handle_upload_to_s3(WorkDir, S3Dir, DirNamesToUpload, Filepath),


	%
	% cleanup
	%
	handle_cleanup(WorkDir, Filepath),


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

handle_upload_to_s3(WorkDir, S3Dir, DirNamesToUpload, Filepath) ->

	dig:log("Uploading to S3"),


	%
	% unzip the file in workdir
	%
	{ok, Pwd} = file:get_cwd(),
	helper:cmd("cd ~s; unzip ~s/~s", [WorkDir, Pwd, Filepath]),


	%
	% for each directory, check if exists and upload to s3
	%
	lists:foreach(fun(DirNameToUpload) ->

		dig:log("Processing ... " ++ DirNameToUpload)

	end, DirNamesToUpload),


	dig:log("Uploading to S3 ... ok").




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
