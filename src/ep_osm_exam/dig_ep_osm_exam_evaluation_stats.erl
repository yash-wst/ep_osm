
-module(dig_ep_osm_exam_evaluation_stats).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("itx/include/records_dev.hrl").


-define(ITXAUDIT_LOG_REMINDER_SENT, "osm_evaluator_reminder_sent").

%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	main(wf:q(anptestid)).


main(Id) when Id /= undefined ->
	Url = itx:format("/~p?id=~s", [?MODULE, Id]),
	helper:redirect(Url);
main(_) ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("OSM Exam Evaluation Statistics").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(reset_beyond_days = I) ->
	itf:dropdown(?F(I, "Inactive Days"), itf:options([
		?F('5'),
		?F('10')
	]));


f(reset_booklet_state = I) ->
	itf:dropdown(?F(I, "Reset Booklet State"), itf:options([
		?F(reset_from_active_to_yet_to_start, "Active to Yet To Start"),
		?F(reset_from_moderation_to_completed, "Moderation to Completed")
	])).



%------------------------------------------------------------------------------
% fs
%------------------------------------------------------------------------------

fs(search) -> [
	?COREXS(season_fk),
	?CORFAC(faculty_code_fk),
	?CORPGM(program_code_fk),
	?CORSUB(subject_code_fk),
	fields:get(anptestcourseid),
	fields:get(teststatus),
	fields:get(exam_pattern),
	itf:build(itf:hidden(osm_exam_fk), itxcontext:q(id))
].


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, ?APPOSM_CONTROLLER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		description="Evaluation Statistics",
		module=?MODULE,
		filters=fs(search),
		events=[
			ite:button(export, "CSV", {itx, {dig, export}})
		],
		actions=[
			{show_send_reminder, "Send Reminder", "Send Reminder"},
			{show_reset_booklet_state, "Reset Booklet State", "Reset Booklet State"}
		],
		size=25
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exam Evaluation Statistics").



%------------------------------------------------------------------------------
% function - init
%------------------------------------------------------------------------------
init() ->
	ok.


%------------------------------------------------------------------------------
% function - fetch
%------------------------------------------------------------------------------


%..............................................................................
%
% []
%
%..............................................................................
fetch(D, From, Size, []) ->
	Fs = [
		fields:build(teststatus, ?ACTIVE)
	],

	fetch(D, From, Size, Fs);



%..............................................................................
%
% _
%
%..............................................................................

fetch(D, From, Size, Fs) ->

	%
	% init
	%
	Today = helper:date_today_str(),
	TodaySeconds = helper:date_d2epoch(Today),
	Size1 = case itxcontext:q(size) of
		undefined ->
			Size;
		Size0 ->
			?S2I(Size0)
	end,


	%
	% get active tests
	%
	Docs = ep_osm_exam_api:fetch(From, Size1, Fs),


	%
	% build dicts
	%
	{SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict} =
		ep_core_helper:get_sfps_dicts(Docs),


	%
	% get stats
	%
	AllStats = get_evaluation_stats0(Docs),




	%
	% results
	%
	Results = lists:map(fun({Doc, Stats}) ->


		%
		% init
		%
		SFPSCells = ep_core_dig_helper:get_sfps_cells(
			Doc, {SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict},
			#dcell {show_ui=false}
		),


		%
		% get stats for test
		%
		StatsDict = dict:from_list(Stats),



		%
		% layout test
		%
		SFPSCells ++ [
			#dcell {type=label, val=itf:val(Doc, anptestcourseid)},
			#dcell {type=label, val=itf:val(Doc, testname)},
			#dcell {type=label, val=anptests:testtotalmarks(Doc)},
			dcell_days_since_test(TodaySeconds, Doc)

		] ++ lists:map(fun(State) ->
				Val = case dict:find([State], StatsDict) of
				{ok, Val0} ->
					Val0;
				_ ->
					0
				end,
				#dcell {
					bgcolor=get_class(State, Val),
					val=get_link(Doc, State, Val),
					val_export=Val
				}
		end, states()) ++ [
			dcell_exam_actions(Doc)
		]


	end, AllStats),


	%
	% sort results
	%
	ResultsSorted = lists:sort(fun(A, B) ->
		#dcell {val=YetToStartA} = lists:nth(8, A),
		#dcell {val=YetToStartB} = lists:nth(8, B),
		YetToStartA > YetToStartB
	end, Results),


	%
	% header
	%
	Header = [
		#dcell {type=header, show_ui=false, val="Season"},
		#dcell {type=header, show_ui=false, val="Faculty"},
		#dcell {type=header, show_ui=false, val="Program"},
		#dcell {type=header, show_ui=false, val="Subject"},
		#dcell {type=header, val="Exam Id"},
		#dcell {type=header, val="Exam Name"},
		#dcell {type=header, val="Total Marks"},
		#dcell {type=header, val="Days"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State++"_min"))}
	end, states()) ++ [
		#dcell {type=header, val="Action"},
		#dcell {type=header, val="Total"}
	],



	%
	% return
	%
	{
		D#dig {
			total=?INFINITY
		},
		[Header] ++ dig:append_total_cells(ResultsSorted)
	}.



%------------------------------------------------------------------------------
% function - exports
%------------------------------------------------------------------------------
exports() -> [
].



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	dig:dig(?MODULE:get()).


%------------------------------------------------------------------------------
% dcells
%------------------------------------------------------------------------------

dcell_days_since_test(TodaySeconds, Doc) ->
	Testdate = itf:val(Doc, startdate),
	DaysSinceTest = get_days_since_test(TodaySeconds, Testdate),
	#dcell {
		bgcolor=get_class_days_since_test(DaysSinceTest),
		val=[
			#span {text=DaysSinceTest},
			#br {},
			#span {style="font-size: 0.7em; white-space: nowrap;", text=Testdate}
		]
	}.



%
% dcell - exam actions
%
dcell_exam_actions(Doc) ->

	Fs = itf:d2f(Doc, [itf:id()]),
	Links = helper_ui:layout_slinks(anptest, Fs),
	#dcell {
		val_export="",
		val=Links
	}.



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({confirmation_yes, reset_forgotten_active}) ->
	handle_reset_forgotten_active_booklets();

event(reset_booklet_state) ->
	itl:confirmation("Are you sure you want to reset booklet states?", reset_forgotten_active);

event({confirmation_yes, send_reminder}) ->
	handle_send_reminder_confirmed();

event(send_reminder) ->
	handle_send_reminder();

event(show_send_reminder) ->
	handle_show_send_reminder();

event(show_reset_booklet_state) ->
	handle_show_reset_booklet_state();

event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------

%..............................................................................
%
% handle - reset which are forgotten in active state
%
%..............................................................................


%
% reset forgotten - add to queue
%
handle_reset_forgotten_active_booklets() ->

	SearchFs = filters(),
	?ASSERT(
		SearchFs /= [],
		"Please select at least one search filter before proceeding"
	),


	case configs:getbool(process_via_minijob, false) of
		true ->
			handle_reset_forgotten_active_booklets_via_minijob();
		false ->
			handle_reset_forgotten_active_booklets_via_taskqueue()
	end.



%
% minijob
%
handle_reset_forgotten_active_booklets_via_minijob() ->
	Fs = [
		itf:build(f(reset_beyond_days), wf:q(reset_beyond_days)),
		itf:build(f(reset_booklet_state), wf:q(reset_booklet_state))
	] ++ filters(),
	{ok, Doc} = minijob_reset_osm_booklet_state:create_and_run(Fs),
	minijob_status:show_status(Doc).



%
% taskqueue
%
handle_reset_forgotten_active_booklets_via_taskqueue() ->

	%
	% init
	%
	ForgottenDays = ?S2I(wf:q(reset_beyond_days)),
	{FromState, ToState} = get_reset_from_to_states(wf:q(reset_booklet_state)),
	Context = wf_context:context(),
	itl:modal_close(),


	%
	% get active tests
	%
	SearchFs = filters(),
	Docs = ep_osm_exam_api:fetch(0, ?INFINITY, SearchFs),
	dig:log(info, io_lib:format("~p active tests found", [length(Docs)])),



	Fun = fun([]) ->
		wf_context:context(Context),
		handle_reset_forgotten_active_booklets(ForgottenDays, {FromState, ToState}, Docs),
		dig:log(success, "Task completed")
	end,


	%
	% add to queue
	%
	taskqueue:create(Fun, []),
	helper_ui:flash(warning, "Added to queue.", 5).



%
% reset forgotten - exec for each test
%
handle_reset_forgotten_active_booklets(ForgottenDays, {FromState, ToState}, Docs) ->

	%
	% init
	%
	Today = helper:date_today_str(),
	TenDaysBeforeToday = helper:date_str_offset(Today, -ForgottenDays),



	%
	% for each test, send reminder
	%
	lists:foreach(fun(Doc) ->

		%
		% init
		%
		Testname = io_lib:format("~s / ~s", [
			itf:val(Doc, anptestcourseid),
			itf:val(Doc, testname)
		]),
		dig:log(warning, "Processing " ++ Testname),


		#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
			anpcandidates:db(itf:idval(Doc)), [
				fields:build(anpstate, FromState)
			], 0, ?INFINITY
		),
		handle_reset_forgotten_active_booklets(Doc, TenDaysBeforeToday, CandidateDocs, {FromState, ToState})
	end, Docs).



%
% reset forgotten - identify docs
%
handle_reset_forgotten_active_booklets(_ExamDoc, _TenDaysBeforeToday, [], _States) ->
	skip;
handle_reset_forgotten_active_booklets(ExamDoc, TenDaysBeforeToday, CandidateDocs, {FromState, ToState}) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(itf:idval(ExamDoc)),

	%
	% filter out docs where last comment is older than 10 days from today
	%
	CandidateDocsToReset = lists:filter(fun(CandidateDoc) ->

		%
		% get last comment date
		%
		Comments = itf:val(CandidateDoc, fields:get(comments)),
		case Comments of
			[] ->
				false;
			_ ->
				{Signature, _Comment} = lists:last(Comments),
				[Date, _Time, _IP, _Role] = string:tokens(Signature, " "),
				[YYYY, MM, DD] = string:tokens(Date, "/"),
				LastCommentDate = string:join([YYYY, MM, DD], "-"),
				LastCommentDate < TenDaysBeforeToday
		end
	end, CandidateDocs),
	handle_reset_forgotten_active_booklets_reset(ExamDb, CandidateDocsToReset, {FromState, ToState}).



%
% reset forgotten - reset
%
handle_reset_forgotten_active_booklets_reset(_ExamDb, [], _States) ->
	skip;
handle_reset_forgotten_active_booklets_reset(ExamDb, CandidateDocsToReset, {_FromState, ToState}) ->
	%
	% reset
	%
	dig:log(warning, io_lib:format("~p booklets will be reset", [length(CandidateDocsToReset)])),
	LoLFs = lists:map(fun(CandidateDoc) ->
		Fs = helper_api:doc2fields({ok, CandidateDoc}),
		Fs1 = fields:listdelete(Fs, get_fs_to_reset(ToState) ++ [
			anpstate
		]),
		Fs1 ++ [
			fields:build(anpstate, ToState)
		]
	end, CandidateDocsToReset),

	%
	% save
	%
	{ok, SaveRes} = anpcandidates:updateall(ExamDb, LoLFs),
	{Oks, Errors} = db_helper:bulksave_summary(SaveRes),
	dig:log(success, io_lib:format("Oks: ~p, Errors: ~p", [Oks, Errors])).



%..............................................................................
%
% handle - send reminder
%
%..............................................................................

handle_send_reminder_confirmed() ->

	%
	% init
	%
	"profiletype_" ++ ProfileType = wf:q(osm_profiletype),
	RoleId = ?L2A(ProfileType),


	case configs:getbool(process_via_minijob, false) of
		false ->
			itl:modal_close(),
			handle_send_reminder_confirmed_via_taskqueue(RoleId);
		true ->
			handle_send_reminder_confirmed_via_minijob(RoleId)
	end.



handle_send_reminder_confirmed_via_minijob(RoleId) ->
	{ok, Doc} = minijob_send_reminders_to_osm_evaluators:create_and_run([
		fields:build(osm_profiletype, wf:q(osm_profiletype))
	]),
	minijob_status:show_status(Doc).


handle_send_reminder_confirmed_via_taskqueue(RoleId)->
	Context = wf_context:context(),
	Fun = fun([]) ->
		wf_context:context(Context),
		handle_send_reminder_confirmed(RoleId),
		dig:log(success, "Task completed")
	end,


	%
	% add to queue
	%
	taskqueue:create(Fun, []),
	helper_ui:flash(warning, "Added to queue.", 5).



handle_send_reminder_confirmed(anpevaluator) ->
	handle_send_reminder_confirmed(anpevaluator, "anpstate_yettostart");
handle_send_reminder_confirmed(anpmoderator) ->
	handle_send_reminder_confirmed(anpmoderator, "anpstate_moderation");
handle_send_reminder_confirmed(anprevaluator) ->
	handle_send_reminder_confirmed(anprevaluator, "anpstate_revaluation");
handle_send_reminder_confirmed(anpmoderator_reval) ->
	handle_send_reminder_confirmed(anpmoderator_reval, "anpstate_moderation_reval").


handle_send_reminder_confirmed(RoleId, AnpCheckState) ->

	%
	% get active tests
	%
	Docs = ep_osm_exam_api:fetch(0, ?INFINITY, [fields:build(teststatus, ?ACTIVE)]),
	dig:log(info, io_lib:format("~p active tests found", [length(Docs)])),

	%
	% create audit before sending
	%
	{ok, _} = itxaudit_api:create(#itxaudit {
		log=?ITXAUDIT_LOG_REMINDER_SENT,
		refid1=?A2L(?MODULE),
		refid2=helper:date_today_str(),
		refid3=minijobcontext:q(osm_profiletype)
	}),



	%
	% for each test, send reminder
	%
	lists:foreach(fun(Doc) ->

		%
		% init
		%
		Testname = io_lib:format("~s / ~s", [
			itf:val(Doc, anptestcourseid),
			itf:val(Doc, testname)
		]),
		dig:log(warning, "Processing " ++ Testname),


		#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
			anpcandidates:db(itf:idval(Doc)),
			[fields:build(anpstate, AnpCheckState)]
		),
		handle_send_reminder_confirmed(RoleId, Doc, CandidateDocs, AnpCheckState)
	end, Docs).




%..............................................................................
%
% handle - send reminder confirmed doc
%
%..............................................................................

handle_send_reminder_confirmed(_RoleId, _Doc, [], AnpCheckState) ->
	dig:log(info, "Skipping, no papers in " ++ ?LN(?L2A(AnpCheckState)));
handle_send_reminder_confirmed(RoleId, Doc, _, _AnpCheckState) ->

	%
	% init
	%
	TFs = helper_api:doc2fields({ok, Doc}),
	SubjectId = itf:val(TFs, subject_code_fk),
	SubjectName = case itf:val(TFs, subject_code_fk) of
		[] ->
			itf:val(TFs, anptestcourseid);
		_ ->
			{ok, SubjectDoc} = ep_core_subject_api:get(SubjectId),
			itf:val(SubjectDoc, subject_code)
	end,


	%
	% get profile ids
	%
	ProfileIds = anpcandidates:get_evaluators_for_test(TFs, RoleId),
	ProfileIdsUnique = helper:unique(ProfileIds),
	dig:log(danger, io_lib:format("Sending ~p SMSes", [length(ProfileIdsUnique)])),


	%
	% get profile docs
	%
	ProfileDocs = profiles:getdocs_by_ids(ProfileIdsUnique),


	%
	% send notification
	%
	lists:foreach(fun(ProfileDoc) ->
		anptest:send_reminder_sms_to_evaluator(TFs, ProfileDoc, SubjectName)
	end, ProfileDocs).



%..............................................................................
%
% handle - send reminder
%
%..............................................................................

handle_send_reminder() ->

	%
	% init
	%
	Date = helper:date_today_str(),
	ProfileType = wf:q(osm_profiletype),


	%
	% check if reminder already sent today
	%
	#db2_find_response {docs=AuditDocs} = db2_find:get_by_fs(
		itxaudit_api:db(), [
			itf:build(?ITXAUD(log), ?ITXAUDIT_LOG_REMINDER_SENT),
			itf:build(?ITXAUD(refid1), ?A2L(?MODULE)),
			itf:build(?ITXAUD(refid2), Date),
			itf:build(?ITXAUD(refid3), ProfileType)
		]
	),



	case length(AuditDocs) < itxconfigs_cache:get2(ep_osm_evaluation_reminders, 2) of
		true ->
			%
			% get active tests
			%
			Docs = ep_osm_exam_api:fetch(0, ?INFINITY, [fields:build(teststatus, ?ACTIVE)]),
			itl:confirmation(
				io_lib:format("Are you sure you want to send reminder for ~p exams?", [length(Docs)]),
				send_reminder
			);
		_ ->
			itl:modal_fs(#panel {
				class="mycenter it-section",
				body=[
					#p {
						text=io_lib:format("Reminders already sent today to ~s. You can send only once per day", [
							?LN(?L2A(ProfileType))
						])
					}
				]
			})
	end.


%..............................................................................
%
% handle - show send reminder
%
%..............................................................................

handle_show_send_reminder() ->

	%
	% build
	%
	Fs = [
		fields:get(osm_profiletype)
	],
	Es = itl:get(?CREATE, Fs, ite:get(send_reminder, "Send"), table),


	%
	% layout
	%
	dig_mm:handle_show_action("Send Reminder", Es).



%..............................................................................
%
% handle - show reset booklet state
%
%..............................................................................

handle_show_reset_booklet_state() ->

	%
	% build
	%
	Fs = [
		f(reset_booklet_state),
		f(reset_beyond_days)
	],
	Es = itl:get(?CREATE, Fs, ite:get(reset_booklet_state, "Reset State"), table),


	%
	% layout
	%
	dig_mm:handle_show_action("Reset Booklet State", Es).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


%
% states
%
states() ->
	lists:map(fun(State) ->
		?A2L(State)
	end, helper_options:options(anpstate)).



%
% get class
%
get_class(State, Number) when Number > 0 ->
	case State of
		"anpstate_not_uploaded" ->
			"table-info";
		"anpstate_yettostart" ->
			"table-warning";
		"anpstate_active" ->
			"table-danger";
		"anpstate_completed" ->
			"table-success";
		"anpstate_moderation" ->
			"table-danger";
		"anpstate_moderation_completed" ->
			"table-success";
		"anpstate_revaluation" ->
			"table-danger";
		"anpstate_revaluation_completed" ->
			"table-success";
		"anpstate_moderation_reval" ->
			"table-danger";
		"anpstate_moderation_reval_completed" ->
			"table-success";
		"anpstate_evaluation_rejected" ->
			"table-danger";
		"anpstate_discarded" ->
			"table-info"
	end;


get_class(_, _) ->
	[].



%
% get link
%
get_link(Doc, State, Number) when Number > 0 ->
	#link {
		new=true,
		text=Number,
		url=itx:format("/~p?id=~s&state=~s", [
			dig_ep_osm_exam_verification, itf:idval(Doc), State
		])
	};
get_link(_, _, Number) ->
	Number.



%
% get evaluation stats
%
get_evaluation_stats0(TestDocs) ->
	lists:map(fun(TestDoc) ->
		TestId = itf:idval(TestDoc),
		{TestDoc, ep_osm_exam_api:get_evaluation_stats0(TestId)}
	end, TestDocs).




%
% get days since test
%
get_days_since_test(_TodaySeconds, []) ->
	[];
get_days_since_test(TodaySeconds, TestDate) ->
	try
		TestdateSeconds = helper:date_d2epoch(TestDate),
		DiffSeconds = TodaySeconds - TestdateSeconds,
		DiffSeconds div (60*60*24)
	catch _:_ ->
		error
	end.




%
% get class days since test
%
get_class_days_since_test(DaysSinceTest) when DaysSinceTest > 45 ->
	"table-danger";
get_class_days_since_test(DaysSinceTest) when DaysSinceTest > 35 ->
	"table-warning";
get_class_days_since_test(DaysSinceTest) when DaysSinceTest > 25 ->
	"table-info";
get_class_days_since_test(_) ->
	"".



%
% get reset states
%
get_reset_from_to_states("reset_from_active_to_yet_to_start") ->
	{"anpstate_active", "anpstate_yettostart"};
get_reset_from_to_states("reset_from_moderation_to_completed") ->
	{"anpstate_moderation", "anpstate_completed"}.



%
% get filters
%
filters() ->
	Dig = helper:state(dig),
	dig:get_nonempty_fs(Dig#dig.filters).



%
% get fs to reset
%
get_fs_to_reset("anpstate_yettostart") ->
	anpcandidate:fids_reset();
get_fs_to_reset(_) ->
	[].



%
% number of reminders sent today
%
number_of_reminders_sent_today() -> 

	%
	% init
	%
	Date = helper:date_today_str(),


	%
	% check if reminder already sent today
	%
	#db2_find_response {docs=AuditDocs} = db2_find:get_by_fs(
		itxaudit_api:db(), [
			itf:build(?ITXAUD(log), ?ITXAUDIT_LOG_REMINDER_SENT),
			itf:build(?ITXAUD(refid1), ?A2L(?MODULE)),
			itf:build(?ITXAUD(refid2), Date)
		], 0, ?INFINITY, [
			{use_index, ["refid2"]}
		]
	),
	length(AuditDocs).
%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
