
-module(ep_osm_exam_api).
-compile(export_all).
-include("records.hrl").

db() ->
	"anptests".

%------------------------------------------------------------------------------
% get
%------------------------------------------------------------------------------
get(Id) ->
	db:get(db(), Id).

%
% assuming a view for the specified field exists
%
get_by_field(F = #field {}) ->
	itxview:find_docs(db(), F).



getdocs() ->
	db:getdocs(db()).


getdocs_by_ids(ExamIds) ->
	db:get_docs_by_ids(db(), ExamIds).


getdocs_by_teststatus(Status) ->
	FsFind = [
		fields:build(teststatus, Status)
	],
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		db(), FsFind, 0, ?INFINITY, [{use_index, ["teststatus"]}]
	),
	Docs.


getdoc_from_cache(Id) ->
	Fn = fun() ->
		ep_osm_exam_api:get(Id)
	end,
	itxdoc_cache:get({?MODULE, Id}, Fn, 10).




list() ->
	lists:map(fun(D) ->
		{helper:l2a(itf:idval(D)), itf:idval(D)}
	end, db:getdocs(db())).


get_pending_states() -> [
	"anpstate_not_uploaded",
	"anpstate_quality_check",
	"anpstate_on_hold",
	"anpstate_yettostart",
	"anpstate_active",
	"anpstate_moderation",
	"anpstate_revaluation",
	"anpstate_moderation_reval",
	"anpstate_evaluation_rejected"
].


%------------------------------------------------------------------------------
% check
%------------------------------------------------------------------------------
exists(F = #field {}) ->
	{Count, _} = get_by_field(F),
	Count > 0.


%------------------------------------------------------------------------------
% save
%------------------------------------------------------------------------------

create(Fs) ->
	anptests:create(Fs).


save(Fields) ->
	db:save(db(), helper_api:fields2doc(Fields)).

save(FsToSave, FsAll, Id) ->
	{ok, Doc} = ?MODULE:get(Id),
	FsAll1 = itf:d2f(Doc, FsAll),
	FsAll2 = itf:fs_merge(FsAll1, FsToSave),
	?MODULE:save(FsAll2).

savebulk(LoLofFields) ->

	%
	% save docs
	%
	Docs = lists:map(fun(Fs) ->
		helper_api:fields2doc(Fs)
	end, LoLofFields),
	{ok, ResDocs} = db:savebulk(db(), Docs),


	%
	% setup
	%
	lists:foreach(fun(Doc) ->
		Id = itf:val(Doc, id),
		anptests:setup(Id)
	end, ResDocs),



	%
	% return
	%
	{ok, ResDocs}.


%------------------------------------------------------------------------------
% mutate
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fetch
%------------------------------------------------------------------------------

fetch(From, Size, Filters) ->
	fetch(From, Size, Filters, []).
fetch(From, Size, Filters, Configs) ->
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		db(), Filters, From, Size, Configs
	),
	Docs.


%------------------------------------------------------------------------------
% delete
%------------------------------------------------------------------------------
delete_by_field(F = #field {}) ->
	{_, Docs} = get_by_field(F),
	lists:map(fun(D) ->
		db:delete_id(db(), itf:idval(D))
	end, Docs).




%------------------------------------------------------------------------------
% stats
%------------------------------------------------------------------------------


get_stats([]) ->
	lists:foldl(fun(State, Acc) ->
		SK = [helper:l2b(State), <<"">>, <<"">>, <<"">>, <<"">>],
		EK = [helper:l2b(State), <<"z\\ufff0">>, <<"z\\ufff0">>, <<"z\\ufff0">>, <<"z\\ufff0">>],
		Acc ++ itxview:get_stats(
			db(), "teststatus_season_fk_faculty_code_fk_program_code_fk_subject_code_fk", SK, EK, 2
		)
	end, [], states());


get_stats([
	#field {id=season_fk, uivalue=SeasonId}
]) ->
	lists:foldl(fun(State, Acc) ->
		SK = [helper:l2b(State), helper:l2b(SeasonId), <<"">>, <<"">>, <<"">>],
		EK = [helper:l2b(State), helper:l2b(SeasonId), <<"z\\ufff0">>, <<"z\\ufff0">>, <<"z\\ufff0">>],
		Acc ++ itxview:get_stats(
			db(), "teststatus_season_fk_faculty_code_fk_program_code_fk_subject_code_fk", SK, EK, 3
		)
	end, [], states());


get_stats([
	#field {id=season_fk, uivalue=SeasonId},
	#field {id=faculty_code_fk, uivalue=FacultyId}
]) ->
	lists:foldl(fun(State, Acc) ->
		SK = [helper:l2b(State), helper:l2b(SeasonId), helper:l2b(FacultyId), <<"">>, <<"">>],
		EK = [helper:l2b(State), helper:l2b(SeasonId), helper:l2b(FacultyId), <<"z\\ufff0">>, <<"z\\ufff0">>],
		Acc ++ itxview:get_stats(
			db(), "teststatus_season_fk_faculty_code_fk_program_code_fk_subject_code_fk", SK, EK, 4
		)
	end, [], states());


get_stats([
	#field {id=season_fk, uivalue=SeasonId},
	#field {id=faculty_code_fk, uivalue=FacultyId},
	#field {id=program_code_fk, uivalue=ProgramId}
]) ->
	lists:foldl(fun(State, Acc) ->
		SK = [helper:l2b(State), helper:l2b(SeasonId), helper:l2b(FacultyId), helper:l2b(ProgramId), <<"">>],
		EK = [helper:l2b(State), helper:l2b(SeasonId), helper:l2b(FacultyId), helper:l2b(ProgramId), <<"z\\ufff0">>],
		Acc ++ itxview:get_stats(
			db(), "teststatus_season_fk_faculty_code_fk_program_code_fk_subject_code_fk", SK, EK, 5
		)
	end, [], states()).



%------------------------------------------------------------------------------
% stats - evaluation
%------------------------------------------------------------------------------

%
% get consolidated evaluation stats of active tests
%
get_stats_consolidated_evaluation_count_of_active_tests() ->

	%
	% get tests
	%
	Tests = anptests:active(),


	%
	% acc
	%
	DictFin = lists:foldl(fun(T, Acc) ->

		%
		% get test stats
		%
		TestId = fields:getuivalue(T, '_id'),
		TestStats = get_evaluation_stats0(TestId),
		
		lists:foldl(fun({[State], Count}, Acc1) ->
			dict:update_counter(State, Count, Acc1)
		end, Acc, TestStats)


	end, dict:new(), Tests),


	%
	% return
	%
	dict:to_list(DictFin).



get_evaluation_stats0(TestId) ->
	SK = [<<"">>, <<"">>],
	EK = [<<"z\\ufff0">>, <<"z\\ufff0">>],

	try
		itxview:get_stats(
			anpcandidates:db(TestId), "state_assigned", SK, EK, 1
		)
	catch error:{badmatch,{error,not_found}} ->
		anptests:setup(TestId),
		itxview:get_stats(
			anpcandidates:db(TestId), "state_assigned", SK, EK, 1
		)
	end.


get_evaluation_stats0_count_by_states(TestId, ListOfListOfStates) ->
	Stats = get_evaluation_stats0(TestId),
	StatsDict = dict:from_list(Stats),
	lists:map(fun(States) ->
		lists:foldl(fun(State, Acc) ->
			Key = [State],
			case dict:find(Key, StatsDict) of
				{ok, Val} ->
					Acc + Val;
				_ ->
					Acc
			end
		end, 0, States)
	end, ListOfListOfStates).




get_evaluation_stats(TestId) ->
	SK = [<<"">>, <<"">>],
	EK = [<<"z\\ufff0">>, <<"z\\ufff0">>],

	lists:foldl(fun(Viename, Acc) ->
		Acc ++
		try
			itxview:get_stats(
				anpcandidates:db(TestId), Viename, SK, EK, 2
			)
		catch error:{badmatch,{error,not_found}} ->
			anptests:setup(TestId),
			itxview:get_stats(
				anpcandidates:db(TestId), Viename, SK, EK, 2
			)
		end
	end, [], [
		"state_assigned",
		"state_assigned_anpmoderator",
		"state_assigned_anprevaluator",
		"state_assigned_anpmoderator_reval"
	]).


get_evaluation_stats(TestId, Role) ->

	SK = [<<"">>, <<"">>],
	EK = [<<"z\\ufff0">>, <<"z\\ufff0">>],

	Viewname = case Role of
		"anpevaluator" ->
			"state_assigned";
		_ ->
			"state_assigned_" ++ Role
	end,

	try
		itxview:get_stats(
			anpcandidates:db(TestId), Viewname, SK, EK, 2
		)
	catch error:{badmatch,{error,not_found}} ->
		anptests:setup(TestId),
		itxview:get_stats(
			anpcandidates:db(TestId), Viewname, SK, EK, 2
		)
	end.



get_evaluator_answerpaper_count(TestId, Role, ProfileId, AnswerPaperState) ->

	%
	% init
	%
	ViewName = case Role of
		"anpevaluator" ->
			"state_assigned";
		_ ->
			"state_assigned_" ++ Role
	end,
	SK = [?L2B(AnswerPaperState), ?L2B(ProfileId)],
	EK = [?L2B(AnswerPaperState), ?L2B(ProfileId)],



	%
	% query
	%
	Res = try
		itxview:get_stats(
			anpcandidates:db(TestId), ViewName, SK, EK, 1
		)
	catch error:{badmatch,{error,not_found}} ->
		anptests:setup(TestId),
		itxview:get_stats(
			anpcandidates:db(TestId), ViewName, SK, EK, 1
		)
	end,


	%
	% return
	%
	case Res of
		[] ->
			0;
		[{[AnswerPaperState], Count}] ->
			Count
	end.


%------------------------------------------------------------------------------
% stats - centre evaluation
%------------------------------------------------------------------------------


get_centre_evaluation_stats(TestId) ->
	SK = [<<"">>, <<"">>],
	EK = [<<"z\\ufff0">>, <<"z\\ufff0">>],

	try
		itxview:get_stats(
			anpcandidates:db(TestId), "code_state", SK, EK, 2
		)
	catch error:{badmatch,{error,not_found}} ->
		anptests:setup(TestId),
		itxview:get_stats(
			anpcandidates:db(TestId), "code_state", SK, EK, 2
		)
	end.


%------------------------------------------------------------------------------
% eva
%------------------------------------------------------------------------------
getstats_evaldate(TestId) ->

	%
	% init
	%
	ViewName = "eval_date_assigned",
	SK = undefined,
	EK = undefined,


	%
	% query
	%
	try
		itxview:get_stats(
			anpcandidates:db(TestId), ViewName, SK, EK, 2
		)
	catch error:{badmatch,{error,not_found}} ->
		anptests:setup(TestId),
		itxview:get_stats(
			anpcandidates:db(TestId), ViewName, SK, EK, 2
		)
	end.

getstats_evaldate_profileid(TestId, Evaldate, ProfileId) ->

	%
	% init
	%
	ViewName = "eval_date_assigned",
	SK = [?L2B(Evaldate), ?L2B(ProfileId)],
	EK = [?L2B(Evaldate), ?L2B(ProfileId)],



	%
	% query
	%
	try
		itxview:get_stats(
			anpcandidates:db(TestId), ViewName, SK, EK, 2
		)
	catch error:{badmatch,{error,not_found}} ->
		anptests:setup(TestId),
		itxview:get_stats(
			anpcandidates:db(TestId), ViewName, SK, EK, 2
		)
	end.


%------------------------------------------------------------------------------
% cap centre stats
%------------------------------------------------------------------------------


%..............................................................................
%
% cap centre test
%
%..............................................................................
get_capcentre_stats_test(TestId, all, GroupLevel) ->
	%
	% init
	%
	Db = anpcandidates:db(TestId),
	Viewname = "ip_state_date_evaluator",
	SK = [<<"">>],
	EK = [<<"z\\ufff0">>],

	try
		itxview:get_stats(Db, Viewname, SK, EK, GroupLevel)
	catch error:{badmatch,{error,not_found}} ->
		anptests:setup(TestId),
		itxview:get_stats(Db, Viewname, SK, EK, GroupLevel)
	end;

get_capcentre_stats_test(TestId, IPs, GroupLevel) ->
	lists:foldl(fun(IP, Acc) ->
		Acc ++ get_capcentre_stats_test_ip(TestId, IP, GroupLevel)
	end, [], IPs).


%..............................................................................
%
% cap centre test ip
%
%..............................................................................

get_capcentre_stats_test_ip(TestId, IP, GroupLevel) ->
	%
	% init
	%
	Db = anpcandidates:db(TestId),
	Viewname = "ip_state_date_evaluator",
	SK = [?L2B(IP), <<"">>, <<"">>, <<"">>],
	EK = [?L2B(IP), <<"z\\ufff0">>, <<"z\\ufff0">>, <<"z\\ufff0">>],


	try
		itxview:get_stats(Db, Viewname, SK, EK, GroupLevel)
	catch error:{badmatch,{error,not_found}} ->
		anptests:setup(TestId),
		itxview:get_stats(Db, Viewname, SK, EK, GroupLevel)
	end.


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


states() -> [
	"new",
	"scheduled",
	"active",
	"completed",
	"retired",
	"redressal"
].



%------------------------------------------------------------------------------
% csv - frp
%------------------------------------------------------------------------------
csv_frp(TestId, "profiletype_" ++ RoleType) ->

	%
	% get candidate docs
	%
	TestDb = anpcandidates:db(TestId),
	CandidateDocs = anpcandidates:getall(TestDb),
	EvaluatorTotalId = ?L2A(?FLATTEN("total_" ++ RoleType)),


	%
	% filter out unwanted docs
	%
	CandidateDocs1 = lists:filter(fun(CDoc) ->
		itf:val(CDoc, anpstate) /= "anpstate_discarded"
	end, CandidateDocs),


	%
	% csv
	% prn, name, marks
	%
	Lines = lists:foldl(fun(CDoc, Acc) ->

		%
		% create string
		%
		Marks = csv_frp_marks(itf:val(CDoc, EvaluatorTotalId)),
		Line = string:join([
			itf:val(CDoc, anpseatnumber),
			itf:val(CDoc, anpfullname),
			Marks
		], ","),


		%
		% do not send ab marks for non-evaluator type
		%
		case {Marks, EvaluatorTotalId} of
			{"ab", total_anpevaluator} ->
				Acc ++ [Line];
			{"ab", _} ->
				Acc;
			_ ->
				Acc ++ [Line]
		end


	end, [], CandidateDocs1),


	%
	% return
	%
	{length(Lines), string:join(Lines, "\n")}.




csv_frp_marks([]) ->
	"ab";
csv_frp_marks(Str) ->
	helper:i2s(helper:ceiling(helper:s2f_v1(Str))).




%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

s3dir(SeasonCode, SubjectCode) ->
	?ASSERT(
		SeasonCode /= [],
		"season code is empty!"
	),

	?ASSERT(
		SubjectCode /= [],
		"season code is empty!"
	),


	S3Dir = ?FLATTEN(io_lib:format("~s/~s/~s", [
		db_domain:host(), SeasonCode, SubjectCode
	])),


	S3Dir.


s3dir_new(SeasonDoc, SubjectDoc, TestId) ->
	SeasonCode = itf:val(SeasonDoc, code),
	SubjectCode = itf:val(SubjectDoc, subject_code),
	case itxconfigs_cache:get2(ep_osm_exam_s3dir_format, undefined) of
		"uid" ->
			ep_osm_exam_api:s3dir(
				itf:idval(SeasonDoc), TestId
			);
		_ ->
			ep_osm_exam_api:s3dir(
				SeasonCode, SubjectCode
			)
	end.




%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

