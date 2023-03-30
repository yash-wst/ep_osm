-module(ep_osm_eval_v2_camera).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%------------------------------------------------------------------------------
% layout
%------------------------------------------------------------------------------

layout_camera(Fs) ->
	layout_camera(Fs, ep_osm_config:is_evaluation_face_proctored()).

layout_camera(_Fs, true) ->

	%
	% camera es
	%
	wf:wire("Webcam.set({width: 160, height: 120});"),
	Es = [
		#panel {id=my_camera, body="Loading ..."},
		#hidden {id=my_camera_image}
	],
	wf:wire(#event{type=timer, delay=100, postback=camera_load, delegate=?MODULE}),


	%
	% schedule proctoring
	%
	schedule_proctor(),


	%
	% fix layout
	%
	#panel {
		style="z-index: 1001; right: 6em;",
		class="bg-white position-fixed bottom-0 mb-2 me-1 p-1 border border-dark",
		body=Es
	};
layout_camera(_Fs, false) ->
	[].



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(proctor) ->
	handle_capture_camera();


event(camera_captured) ->
	schedule_proctor(),
	handle_verify_face();


event(camera_load) ->
	wf:wire("Webcam.attach('.wfid_my_camera')").



%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - capture camera
%
%..............................................................................

handle_capture_camera() ->
	wf:wire("Webcam.snap(function(data_uri) {
		$('.wfid_my_camera_image').val(data_uri)
	});"),
	wf:wire(#event{type=timer, delay=100, delegate=?MODULE, postback=camera_captured}).


%..............................................................................
%
% handle - verify face
%
%..............................................................................

handle_verify_face() ->

	%
	% init
	%
	AnpId = wf:q(anpcandidate:id()),
	EvaluatorId = myauth:profileid(),
	"data:image/jpeg;base64," ++ Base64Data = wf:q(my_camera_image),
	CamPhoto = base64:decode(Base64Data),
	ProfilePhoto = attachment:get_content(
		profiles:getdb(), EvaluatorId, "profile_photo.jpg"
	),


	%
	% match faces
	%
	handle_save_captured_image(AnpId, EvaluatorId, wf:q(my_camera_image)),
	Res = itxface_awsapi:compare_faces(CamPhoto, ProfilePhoto),
	handle_verify_face_res(
		EvaluatorId, element(1, Res), ep_osm_config:evaluation_face_proctor_action()
	).



%..............................................................................
%
% handle - verify face res
%
%..............................................................................

handle_verify_face_res(EvaluatorId, true, _) ->
	ok;

handle_verify_face_res(_EvaluatorId, false, "warn") ->
	helper_ui:flash(error, "Face recognition failed!");

handle_verify_face_res(_EvaluatorId, false, "logout") ->
	wf:redirect("/itxlogout");

handle_verify_face_res(EvaluatorId, false, "suspend_and_logout") ->
	{ok, _} = profiles:suspend_profile(EvaluatorId),
	wf:redirect("/itxlogout").




%..............................................................................
%
% handle - save camera image
%
%..............................................................................

handle_save_captured_image(AnpId, EvaluatorId, "data:image/jpeg;base64," ++ Base64Data) ->

	%
	% init
	%
	CamPhoto = base64:decode(Base64Data),
	S3Key = ?FLATTEN(io_lib:format("~s/~s/~s/~s/~s.jpg", [
		db_domain:host(), "__PROCTORING__", AnpId, EvaluatorId, helper:uidintstr()
	])),
	?D(S3Key).





%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

schedule_proctor() ->
	IntervalSecs = ep_osm_config:evaluation_face_proctoring_interval_secs(),
	Timeout = 60 + random:uniform(IntervalSecs),
	wf:wire(#event{
		type=timer, delay=Timeout*1000, delegate=?MODULE, postback=proctor
	}).

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
