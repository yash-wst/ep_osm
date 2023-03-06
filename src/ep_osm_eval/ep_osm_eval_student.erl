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

	?ASSERT(itf:val2(TFs, student_ans_booklet_access) == ?YES, ?LN("Can not access booklet. Please contact college admin.")),

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
	layout:grow(layout:g(12, anpcandidate_answerpaper:layout_answerpaper(TFs, Fs))).


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
	AnpState = itxconfigs_cache:get2(ep_osm_eval_student_anpstate, "anpstate_moderation"),
	FsToSave = [
		fields:build(anpstate, AnpState),
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


%
% get anpid anptestid
%
get_anptestid_anpid(SeasonId, SubjectId, PRN) ->
	get_anptestid_anpid(SeasonId, SubjectId, PRN, []).

get_anptestid_anpid(SeasonId, SubjectId, PRN, MType) ->
	%
	% get osm exam by season id and subject id
	%
	{ok, SubjectDoc} = ep_core_subject_api:get(SubjectId),
	FsFind = [
		fields:build(season_fk, SeasonId),
		fields:build(anptestcourseid, itf:val(SubjectDoc, subject_code)),
		fields:build(exam_pattern, itf:val(SubjectDoc, pattern))
	],
	ExamDocs =ep_osm_exam_api:fetch(0, 10, FsFind, [
		{use_index, ["season_fk"]}
	]),

	FExamDocs = case ExamDocs of
		[ED] -> [ED];
		[] -> [];
		_ -> lists:filter(fun(Doc) -> itf:val2(Doc, marktype) == MType end, ExamDocs)
	end,

	get_anptestid_anpid1(SeasonId, SubjectId, PRN, FExamDocs).


get_anptestid_anpid1(_SeasonId, _SubjectId, PRN, [ExamDoc]) ->
	%
	% get candidate doc
	%
	ExamId = itf:idval(ExamDoc),
	case anpcandidates:get_by_sno(ExamId, PRN) of
		{error, not_found} ->
			{error, candidate_not_found};
		CFs ->
			{ExamId, itf:val(CFs, '_id')}
	end;
get_anptestid_anpid1(_SeasonId, _SubjectId, _PRN, Docs) ->
	{error, Docs}.





%------------------------------------------------------------------------------
% handle
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
