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
		layout_student_grievance(TFs, Fs),
		layout_marking_scheme(TFs, Fs),
		layout_answerpaper(TFs, Fs)
	]}).



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------


%..............................................................................
%
% layout - student info
%
%..............................................................................

layout_student_info(_TFs, Fs) ->
	Es = layout:get(?VIEW, fields:getfields(Fs, [
		anpseatnumber,
		anpstate
	]), [], table),
	[
		#p {class="font-weight-bold", text="Student Info"},
		Es
	].


%..............................................................................
%
% layout - marking scheme
%
%..............................................................................

layout_marking_scheme(TFs, Fs) ->
	layout:g(4, anpcandidate:layout_evaluator_marking_0(
		TFs, Fs, anpevaluator)).


%..............................................................................
%
% layout answer paper
%
%..............................................................................

layout_answerpaper(TFs, Fs) ->
	itl:wire_script("ANP.disable_selection();"),
	layout:grow(layout:g(12, anpcandidate:layout_answerpaper(TFs, Fs))).


%..............................................................................
%
% layout - student grievance
%
%..............................................................................

layout_student_grievance(TFs, Fs) ->
	layout_student_grievance(TFs, Fs, itf:val(Fs, anp_redressal_state)).

layout_student_grievance(_TFs, Fs, RedressalState) when
	RedressalState == "new";
	RedressalState == [] ->

	%
	% init
	%
	FGrievance = fields:build(
		anp_redressal_grievance,
		itf:val(Fs, anp_redressal_grievance)
	),
	FsEdit = [
		FGrievance#field {validators=[required, length_limit_500]}
	],


	%
	% layout
	%
	Event = ite:get(submit_redressal, "Submit Redressal"),
	itl:get(?EDIT, FsEdit, Event, table);

layout_student_grievance(_TFs, Fs, _) ->
	FsView = itf:find(itf:toui(Fs), [
		anp_redressal_state,
		anp_redressal_grievance
	]),
	itl:get(?VIEW, FsView, noevent, table).


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({confirmation_yes,submit_redressal}) ->

	%
	% init
	%
	OsmExamId = wf:q(anptestid),
	CandidateId = wf:q(anpid),
	ExamDb = anpcandidates:db(OsmExamId),
	{ok, Doc} = anpcandidates:getdoc(ExamDb, CandidateId),

	%
	% update
	%
	FsToSave = [
		fields:build(anp_redressal_state, "submitted")
	] ++ itf:uivalue([
		fields:get(anp_redressal_grievance)
	]),
	{ok, _} = ep_osm_candidate_api:update(OsmExamId, Doc, FsToSave),
	helper:redirect(wf:uri());


event(submit_redressal) ->
	itl:confirmation(
		"Are you sure you want to sumit? Grievance cannot be changed after submission",
		submit_redressal
	);

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
