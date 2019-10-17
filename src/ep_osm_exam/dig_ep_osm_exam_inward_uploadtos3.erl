
-module(dig_ep_osm_exam_inward_uploadtos3).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------


upload(Filename, Fileloc) ->

	%
	% verify file is zip
	%
	handle_verify_zip_file(),


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
% handle - verify zip file
%------------------------------------------------------------------------------

handle_verify_zip_file() ->
	dig:log("Verifying file is zip file"),

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
