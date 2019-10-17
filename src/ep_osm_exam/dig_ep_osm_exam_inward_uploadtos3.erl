
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
	% upload to s3
	%
	handle_upload_to_s3(),


	%
	% cleanup
	%
	handle_cleanup(),


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

handle_upload_to_s3() ->

	dig:log("Uploading to S3"),

	dig:log("Uploading to S3 ... ok").




%------------------------------------------------------------------------------
% handle - cleanup
%------------------------------------------------------------------------------

handle_cleanup() ->

	dig:log("Cleaning up"),

	dig:log("Cleaning up ... ok").



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
