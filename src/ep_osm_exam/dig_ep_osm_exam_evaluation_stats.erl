
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
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Exam Evaluation Statistics").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_CONTROLLER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		description="Evaluation Statistics",
		module=?MODULE,
		filters=[
			?COREXS(season_fk),
			?CORFAC(faculty_code_fk),
			?CORPGM(program_code_fk),
			?CORSUB(subject_code_fk),
			fields:get(anptestcourseid),
			fields:get(teststatus),
			fields:get(exam_pattern),
			itf:build(itf:hidden(osm_exam_fk), wf:q(id))
		],
		events=[
			ite:button(export, "CSV", {itx, {dig, export}})
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
% [osm_exam_fk]
%
%..............................................................................
fetch(D, _From, _Size, [
	#field {id=osm_exam_fk, uivalue=ExamId}
	]) ->


	%
	% init
	%
	TFs = anptests:get(ExamId),


	%
	% get evaluation stats
	%
	Stats = ep_osm_exam_api:get_evaluation_stats(ExamId),
	StatsDict = dict:from_list(Stats),


	%
	% get profile docs
	%
	ProfileIds = lists:map(fun({[_, ProfileId], _}) ->
		ProfileId
	end, Stats),
	AllEligibleProfileIds = ProfileIds ++ anpcandidates:get_evaluators_for_test(TFs, anpevaluator),
	ProfileIdsUnique = helper:unique(AllEligibleProfileIds) -- ["unassigned"],
	ProfileDocs = profiles:getdocs_by_ids(ProfileIdsUnique),
	ProfileDocsDict = helper:get_dict_from_docs(ProfileDocs),



	%
	% layout results
	%
	Results = lists:map(fun(ProfileId) ->

		%
		% init
		%
		ProfileDoc = helper:get_doc_or_empty_doc_from_dict(ProfileId, ProfileDocsDict),
		Role = case itf:val(ProfileDoc, profiletype) of
			"anp" ++ Role0  ->
				Role0;
			_ ->
				"evaluator"
		end,

		%
		% get stats per profile
		%
		[
			#dcell {
				val=#link {
					new=true,
					body=itl:blockquote([
						itf:val(ProfileDoc, fullname),
						itf:val(ProfileDoc, mobile),
						itf:val(ProfileDoc, email)
					]),
					url=io_lib:format("/anptest?mode=status_~s&anptestid=~s&profileid=~s", [
						Role, ExamId, ProfileId
					])
				},
				val_export=io_lib:format("~s / ~s", [
					itf:val(ProfileDoc, mobile),
					itf:val(ProfileDoc, fullname)
				])
			},
			#dcell {
				val=?LN(?L2A(itf:val(ProfileDoc, profiletype)))
			}
		] ++ lists:map(fun(State) ->
			Val = get_eval_count_for_profile(
				ProfileId,
				dict:find([State, ProfileId], StatsDict),
				State,
				itf:val(ProfileDoc, profiletype)
			),
			#dcell {
				bgcolor=get_class(State, Val),
				val=Val
			}
		end, states())

	end, ["unassigned"] ++ ProfileIdsUnique),



	%
	% header
	%
	Header = [
		#dcell {type=header, val="Profile"},
		#dcell {type=header, val="Role"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State++"_min"))}
	end, states()) ++ [
		#dcell {type=header, val="Total"}
	],


	%
	% sort results
	%
	ResultsSorted = lists:sort(fun(A, B) ->
		#dcell {val=YetToStartA} = lists:nth(5, A),
		#dcell {val=YetToStartB} = lists:nth(5, B),
		YetToStartA > YetToStartB
	end, Results),



	%
	% return
	%
	{
		D#dig {
			total=length(ProfileDocs),
			description=itf:val(TFs, testname)
		},
		[Header] ++ tl(dig:append_total_cells(ResultsSorted))
	};


%..............................................................................
%
% []
%
%..............................................................................
fetch(D, From, Size, []) ->
	Fs = [
		fields:build(teststatus, ?ACTIVE)
	],

	{D1, Results} = fetch(D, From, Size, Fs),


	{
		D1#dig {
			actions=[
				{show_send_reminder, "Send Reminder", "Send Reminder"},
				{reset_forgotten_active, "Rest Forgotten Active Booklets", "Rest Forgotten Active Booklets"}
			]
		},
		Results
	};



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
	Size1 = case wf:q(size) of
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
	SeasonDocsDict = ep_core_exam_season_api:get_dict(Docs),
	FacultyDocsDict = ep_core_faculty_api:get_dict(Docs),
	ProgramDocsDict = ep_core_program_api:get_dict(Docs),
	SubjectDocsDict = ep_core_subject_api:get_dict(Docs),


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
		SeasonDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, season_fk), SeasonDocsDict),
		FacultyDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, faculty_code_fk), FacultyDocsDict),
		ProgramDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, program_code_fk), ProgramDocsDict),
		SubjectDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, subject_code_fk), SubjectDocsDict),


		%
		% get stats for test
		%
		StatsDict = dict:from_list(Stats),



		%
		% layout test
		%
		[
			#dcell {
				val=itl:blockquote(SeasonDoc, [?COREXS(name), ?COREXS(state)]),
				val_export=?FLATTEN(itf:val(SeasonDoc, name))
			},
			#dcell {
				val=itl:blockquote(FacultyDoc, [?CORFAC(faculty_code), ?CORFAC(faculty_name)]),
				val_export=itf:val(FacultyDoc, faculty_code) ++ " / " ++ itf:val(FacultyDoc, faculty_name)
			},
			#dcell {
				val=itl:blockquote(ProgramDoc, [?CORPGM(program_code), ?CORPGM(program_name)]),
				val_export=itf:val(ProgramDoc, program_code) ++ " / " ++ itf:val(ProgramDoc, program_name)
			},
			#dcell {
				val=itl:blockquote(SubjectDoc, [?CORSUB(subject_code), ?CORSUB(subject_name)]),
				val_export=itf:val(SubjectDoc, subject_code) ++ " / " ++ itf:val(SubjectDoc, subject_name)
			},
			#dcell {
				val=itl:blockquote([
					#link {
						new=true,
						url=io_lib:format("/~p?id=~s", [
							wf:page_module(), itf:idval(Doc)
						]),
						text=itf:val(Doc, anptestcourseid)
					}
				]),
				val_export=itf:val(Doc, anptestcourseid)
			},
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
					val=Val
				}
		end, states())


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
		#dcell {type=header, val="Season"},
		#dcell {type=header, val="Faculty"},
		#dcell {type=header, val="Program"},
		#dcell {type=header, val="Subject"},
		#dcell {type=header, val="Test Id"},
		#dcell {type=header, val="Days"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State++"_min"))}
	end, states()) ++ [
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


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({confirmation_yes, reset_forgotten_active}) ->
	handle_reset_forgotten_active_booklets();

event(reset_forgotten_active) ->
	itl:confirmation("Are you sure you want to reset forgotten active booklets?", reset_forgotten_active);

event({confirmation_yes, send_reminder}) ->
	handle_send_reminder_confirmed();

event(send_reminder) ->
	handle_send_reminder();

event(show_send_reminder) ->
	handle_show_send_reminder();

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

	%
	% init
	%
	ForgottenDays = 10,
	Context = wf_context:context(),
	itl:modal_close(),

	Fun = fun([]) ->
		wf_context:context(Context),
		handle_reset_forgotten_active_booklets(ForgottenDays),
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
handle_reset_forgotten_active_booklets(ForgottenDays) ->

	%
	% init
	%
	Today = helper:date_today_str(),
	TenDaysBeforeToday = helper:date_str_offset(Today, -ForgottenDays),


	%
	% get active tests
	%
	Docs = ep_osm_exam_api:fetch(0, ?INFINITY, [fields:build(teststatus, ?ACTIVE)]),
	dig:log(info, io_lib:format("~p active tests found", [length(Docs)])),


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
				fields:build(anpstate, "anpstate_active")
			], 0, ?INFINITY
		),
		handle_reset_forgotten_active_booklets(Doc, TenDaysBeforeToday, CandidateDocs)
	end, Docs).



%
% reset forgotten - identify docs
%
handle_reset_forgotten_active_booklets(_ExamDoc, _TenDaysBeforeToday, []) ->
	skip;
handle_reset_forgotten_active_booklets(ExamDoc, TenDaysBeforeToday, CandidateDocs) ->

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
	handle_reset_forgotten_active_booklets_reset(ExamDb, CandidateDocsToReset).



%
% reset forgotten - reset
%
handle_reset_forgotten_active_booklets_reset(_ExamDb, []) ->
	skip;
handle_reset_forgotten_active_booklets_reset(ExamDb, CandidateDocsToReset) ->
	%
	% reset
	%
	dig:log(warning, io_lib:format("~p booklets will be reset", [length(CandidateDocsToReset)])),
	LoLFs = lists:map(fun(CandidateDoc) ->
		Fs = helper_api:doc2fields({ok, CandidateDoc}),
		Fs1 = fields:listdelete(Fs, anpcandidate:fids_reset() ++ [
			anpstate
		]),
		Fs1 ++ [
			fields:build(anpstate, "anpstate_yettostart")
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
	Context = wf_context:context(),
	itl:modal_close(),
	"profiletype_" ++ ProfileType = wf:q(osm_profiletype),
	RoleId = ?L2A(ProfileType),

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
		refid3=wf:q(osm_profiletype)
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



	case AuditDocs of
		[] ->
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
			"bg-info";
		"anpstate_yettostart" ->
			"bg-warning";
		"anpstate_active" ->
			"bg-danger";
		"anpstate_completed" ->
			"bg-success";
		"anpstate_moderation" ->
			"bg-danger";
		"anpstate_moderation_completed" ->
			"bg-success";
		"anpstate_revaluation" ->
			"bg-danger";
		"anpstate_revaluation_completed" ->
			"bg-success";
		"anpstate_moderation_reval" ->
			"bg-danger";
		"anpstate_moderation_reval_completed" ->
			"bg-success";
		"anpstate_evaluation_rejected" ->
			"bg-danger";
		"anpstate_discarded" ->
			"bg-info"
	end;


get_class(_, _) ->
	[].




%
% get evaluation count
%
get_eval_count_for_profile(_, error, _, _) ->
	0;
get_eval_count_for_profile("unassigned", {ok, Val}, State, _) when
	State == "anpstate_not_uploaded";
	State == "anpstate_yettostart";
	State == "anpstate_discarded";
	State == "anpstate_moderation" ->
	Val;
get_eval_count_for_profile("unassigned", _, _, _) ->
	0;
% get_eval_count_for_profile(_, {ok, Val}, State, "anpevaluator") when
% 	State == "anpstate_yettostart";
% 	State == "anpstate_active";
% 	State == "anpstate_completed";
% 	State == "anpstate_evaluation_rejected" ->
% 	Val;
% get_eval_count_for_profile(_, {ok, Val}, State, "anpmoderator") when
% 	State == "anpstate_moderation";
% 	State == "anpstate_moderation_completed" ->
% 	Val;
% get_eval_count_for_profile(_, {ok, Val}, State, "anprevaluator") when
% 	State == "anpstate_revaluation";
% 	State == "anpstate_revaluation_completed" ->
% 	Val;
% get_eval_count_for_profile(_, {ok, Val}, State, "anpmoderator_reval") when
% 	State == "anpstate_moderation_reval";
% 	State == "anpstate_moderation_reval_completed" ->
% 	Val;
get_eval_count_for_profile(_, {ok, Val}, _State, _Role) ->
	Val;
get_eval_count_for_profile(_, _, _, _) ->
	0.



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
	TestdateSeconds = helper:date_d2epoch(TestDate),
	DiffSeconds = TodaySeconds - TestdateSeconds,
	DiffSeconds div (60*60*24).




%
% get class days since test
%
get_class_days_since_test(DaysSinceTest) when DaysSinceTest > 45 ->
	"bg-danger";
get_class_days_since_test(DaysSinceTest) when DaysSinceTest > 35 ->
	"bg-warning";
get_class_days_since_test(DaysSinceTest) when DaysSinceTest > 25 ->
	"bg-info";
get_class_days_since_test(_) ->
	"".



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
