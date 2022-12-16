-module(ep_osm_eval_v2).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


pagejs() -> [
	fabricjs,
	"/lib/ep_osm/priv/static/js/ep_osm_eval_v2.2.js"
].

pagecss() -> [
	"/lib/ep_osm/priv/static/css/ep_osm_eval_v2.2.css"
].


main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/adminkit/html/entered.html"}).

title() ->
	?LN("Evaluation").

heading() ->
	?LN("Evaluation").

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------

access(_, ?APPOSM_EVALUATOR) -> true;
access(_, ?APPOSM_MODERATOR) -> true;
access(_, ?APPOSM_REVALUATOR) -> true;
access(_, ?APPOSM_MODERATOR_REVAL) -> true;
access(_, _) -> false.

%------------------------------------------------------------------------------
% layout
%------------------------------------------------------------------------------
layout() ->

	%
	% init data
	%
	TestId = wf:q(anptest:id()),
	TestFs = anptests:get(TestId),
	Fs = anpcandidates:get(anpcandidate:db(), wf:q(anpcandidate:id())),

	%
	% layout elements
	%
	Elements = [
		ep_osm_eval_v2_navbar:layout_navbar(),

    	ep_osm_eval_v2_toolbar:layout_toolbar(TestFs),

    	ep_osm_eval_v2_marks_box:layout_marks_box(TestFs, Fs),

    	ep_osm_eval_v2_review_area:layout_review_area(TestFs, Fs)
	],

	wf:wire(#api {name=canvas_save, tag=canvas_save}),

	%
	% update progress bar on initial page load
	%
	ep_osm_eval_v2_marks_box:update_progress_bar(),

	Elements.



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event(ALL) ->
	ep_osm_eval_v2_event_handler:event(ALL).


%---------------------------------------------------------------------------------------------------
% API EVENTS
%---------------------------------------------------------------------------------------------------
api_event(canvas_save, canvas_save, [CanvasId, CanvasData]) ->
	Res = anpcandidate:api_event(canvas_save, canvas_save, [CanvasId, CanvasData]),
	ep_osm_eval_v2_marks_box:update_progress_bar(),
	Res;

api_event(X, Y, Z) ->
	anpcandidate:api_event(X, Y, Z).



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
