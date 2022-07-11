-module(ep_osm_eval_student).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


pagejs() -> [
	fabricjs,
	"/lib/ep_osm/priv/static/js/ep_osm_2.js"
].


main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/adminkit/html/entered.html"}).


title() ->
	?LN("Answerbook").

heading() ->
	?LN("Answerbook").



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------

access(_, ?APPOSM_ADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------

layout() ->
	layout(wf:q(anptestid), wf:q(anpid)).


layout(TestId, CandidateId) ->
	%
	% init
	%
	TFs = anptests:get(TestId),
	Fs = anpcandidates:get(anpcandidates:db(TestId), CandidateId),


	%
	% layout
	%
	?AKIT({layout, card_list_group, [
		layout_student_info(TFs, Fs),
		layout:g(4, anpcandidate:layout_evaluator_marking_0(TFs, Fs, anpevaluator)),
		layout_answerpaper(TFs, Fs)
	]}).



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------


%
% layout - student info
%

layout_student_info(_TFs, Fs) ->
	Es = layout:get(?VIEW, fields:getfields(Fs, [
		anpseatnumber,
		anpstate
	]), [], table),
	[
		#p {class="font-weight-bold", text="Student Info"},
		Es
	].



%
% layout answer paper
%
layout_answerpaper(TFs, Fs) ->
	itl:wire_script("ANP.disable_selection();"),
	layout:grow(layout:g(12, anpcandidate:layout_answerpaper(TFs, Fs))).


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
		 
event(Event) ->
	?D(Event).


%------------------------------------------------------------------------------
% event - file
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% handle
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
