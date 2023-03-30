-module(ep_osm_eval_v2_camera).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%------------------------------------------------------------------------------
% layout
%------------------------------------------------------------------------------

layout_camera(Fs) ->
	layout_camera(Fs, ep_osm_config:is_evaluation_face_proctored()).

layout_camera(Fs, true) ->

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
	% fix layout
	%
	#panel {
		style="z-index: 1001; right: 6em;",
		class="bg-white position-fixed bottom-0 mb-2 me-1 p-1 border border-dark",
		body=Es
	};
layout_camera(Fs, false) ->
	[].



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------


event(camera_load) ->
	wf:wire("Webcam.attach('.wfid_my_camera')").




%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
