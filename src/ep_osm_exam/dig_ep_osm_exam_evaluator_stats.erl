
-module(dig_ep_osm_exam_evaluator_stats).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-import(dig_ep_osm_exam_evaluation_stats, [get_class/2, states/0]).

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
	?LN("OSM Evaluator Statistics").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------

-define(BATCH_SIZE, 100).


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
		module=?MODULE,
		filters=[
			?COREXS(season_fk),
			?CORFAC(faculty_code_fk),
			?CORPGM(program_code_fk),
			?CORSUB(subject_code_fk),
			fields:get(anptestcourseid),
			fields:get(teststatus),
			fields:get(exam_pattern),
			itf:build(itf:hidden(osm_exam_fk), itxcontext:q(id)),
			itf:build(itf:hidden(profileid), itxcontext:q(evaluatorid))
		],
		size=25,
		actions=[
			{export_evaluator_stats_bulk, "Bulk Export Evaluator Stats", "Bulk Export Evaluator Stats"}
		],
		events=[
			ite:button(export, "CSV", {itx, {dig, export}})
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Evaluator Statistics").



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
fetch(D, From, Size, [
	#field {id=osm_exam_fk, uivalue=ExamId},
	#field {id=profileid, uivalue=ProfileId}
	]) ->


	%
	% init
	%
	TFs = anptests:get(ExamId),
	PFs = profiles:get(ProfileId),
	Role = itf:val(PFs, profiletype),
	FId = ?L2A("profileidfk_" ++ Role),
	Module = ?L2A("profile_" ++ Role),


	%
	% get docs
	%
	Docs = ep_osm_candidate_api:fetch(
		ExamId, From, Size, [
			fields:build(FId, ProfileId)
		]
	),


	%
	% layout results
	%
	Results = lists:map(fun(CDoc) ->
		[
			#dcell {val=itf:val(CDoc, anp_paper_uid)},
			#dcell {val=itf:val(CDoc, anpseatnumber)},
			#dcell {val=itf:val(CDoc, anpfullname)},
			#dcell {val=?LN(?L2A(itf:val(CDoc, anpstate)))},
			#dcell {
				val=#link {
					text="View",
					new=true,
					url=io_lib:format("/anpcandidate?mode=view&anpid=~s&anptestid=~s", [
						itf:idval(CDoc), ExamId
					])
				}
			},
			#dcell {
				val=#link {
					text="View",
					new=true,
					url=io_lib:format("/ep_osm_eval_view?mode=view&anpid=~s&anptestid=~s&role=anpevaluator", [
						itf:idval(CDoc), ExamId
					])
				}
			}
		]
	end, Docs),



	%
	% header
	%
	Header = [
		#dcell {type=header, val="Barcode / UID"},
		#dcell {type=header, val="Seat No."},
		#dcell {type=header, val="Full name"},
		#dcell {type=header, val="State"},
		#dcell {type=header, val="Candidate Doc"},
		#dcell {type=header, val="Scanned Images"}
	],

	%
	% return
	%
	{
		D#dig {
			dcell_headers=Header,
			description=[
				#link {
					style="margin-right: 15px;",
					url=itx:format("/anptest?mode=view&anptestid=~s", [ExamId]),
					text=io_lib:format("~s / ~ts / ~s", [
						itf:val(TFs, anptestcourseid),
						itf:val(TFs, testname),
						?LN(?L2A(itf:val(TFs, teststatus)))
					])
				},
				#link {
					url=itx:format("/~p?mode=view&profileid=~s", [
						Module, ProfileId
					]),
					text=io_lib:format("~s / ~s", [
						profiles:displayname_fmt(PFs), ?LN(?L2A(Role))
					])
				}
			]
		},
		Results
	};




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
	% spfs cells
	%
	{SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict} =
		ep_core_helper:get_sfps_dicts([TFs]),
	SFPSCells = ep_core_dig_helper:get_sfps_cells(
		TFs, {SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict},
		#dcell {show_ui=false}
	),


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
	AllEligibleProfileIds = ProfileIds ++ 
		anpcandidates:get_evaluators_for_test(TFs, anpevaluator) ++
		anpcandidates:get_evaluators_for_test(TFs, anpmoderator) ++
		anpcandidates:get_evaluators_for_test(TFs, anprevaluator) ++
		anpcandidates:get_evaluators_for_test(TFs, anpmoderator_reval),
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

		%
		% get evaluator link
		%
		EvaluatorLink = case ProfileId of
			"unassigned" ->
				[];
			_ ->
				 #link {
					text="View",
					url=itx:format("~p?id=~s&evaluatorid=~s", [
						?MODULE, ExamId, ProfileId
					])
				}
		end,



		%
		% get stats per profile
		%
		SFPSCells ++ [
			#dcell {type=label, val=itf:val(TFs, anptestcourseid)},
			#dcell {type=label, val=itf:val(TFs, testname)}
			#dcell {val=itf:val(ProfileDoc, fullname)},
			#dcell {type=label, val=itf:val(ProfileDoc, mobile)},
			#dcell {val=itf:val(ProfileDoc, email)},
			#dcell {val=?LN(?L2A(itf:val(ProfileDoc, profiletype)))}
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
		end, states()) ++ [
			#dcell {
				show_csv=false,
				val=EvaluatorLink
			}
		]

	end, ["unassigned"] ++ ProfileIdsUnique),



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
		#dcell {type=header, val="Fullname"},
		#dcell {type=header, val="Mobile"},
		#dcell {type=header, val="Email"},
		#dcell {type=header, val="Role"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State++"_min"))}
	end, states()) ++ [
		#dcell {type=header, val="View", show_csv=false},
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
			description=#link {
				url=itx:format("/anptest?mode=view&anptestid=~s", [ExamId]),
				text=io_lib:format("~s / ~ts / ~s", [
					itf:val(TFs, anptestcourseid),
					itf:val(TFs, testname),
					?LN(?L2A(itf:val(TFs, teststatus)))
				])
			}
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


	%
	% get active tests
	%
	Docs = ep_osm_exam_api:fetch(From, Size, Fs),


	%
	% build dicts
	%
	{SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict} =
		ep_core_helper:get_sfps_dicts(Docs),


	%
	% results
	%
	Results = lists:map(fun(Doc) ->


		%
		% init
		%
		TFs = helper_api:doc2fields({ok, Doc}),
		SFPSCells = ep_core_dig_helper:get_sfps_cells(
			Doc, {SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict},
			#dcell {show_ui=false}
		),



		%
		% get stats
		%
		Stats = ep_osm_exam_api:get_evaluation_stats0(itf:idval(Doc)),
		StatsDict = dict:from_list(Stats),


		%
		% layout test
		%
		SFPSCells ++ [
			#dcell {type=label, val=itf:val(Doc, anptestcourseid)},
			#dcell {type=label, val=itf:val(Doc, testname)}
		] ++ lists:map(fun(RoleId) ->
				ProfileIds = anpcandidates:get_evaluators_for_test(TFs, RoleId),
				ProfileIdsUnique = helper:unique(ProfileIds),
				layout_cell(RoleId, StatsDict, ProfileIdsUnique)
		end, [
			anpevaluator,
			anpmoderator,
			anprevaluator,
			anpmoderator_reval
		]) ++ [
			dig_ep_osm_exam_evaluation_stats:dcell_exam_actions(Doc)
		]


	end, Docs),


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
		#dcell {type=header, val="Number of " ++ ?LN(anpevaluator)},
		#dcell {type=header, val="Number of " ++ ?LN(anpmoderator)},
		#dcell {type=header, val="Number of " ++ ?LN(anprevaluator)},
		#dcell {type=header, val="Number of " ++ ?LN(anpmoderator_reval)},
		#dcell {type=header, val="Action"}
	],



	%
	% return
	%
	{
		D#dig {
			total=?INFINITY
		},
		[Header] ++ Results
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



%..............................................................................
%
% layout cell
%
%..............................................................................

layout_cell(RoleId, StatsDict, ProfileIds) ->

	%
	% init
	%
	TotalPapers = lists:foldl(fun(State, Acc) ->
		Val = dict:find([?A2L(State)], StatsDict),
		Acc + get_val(Val)
	end, 0, states(RoleId)),


	#dcell {
		bgcolor=get_class_evaluator(length(ProfileIds), TotalPapers),
		val=length(ProfileIds)
	}.


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(export_evaluator_stats_bulk) ->
	handle_export_evaluator_stats_bulk();

event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - bulk export evaluator stats
%
%..............................................................................

handle_export_evaluator_stats_bulk() ->

	%
	% init
	%
	D = helper:state(dig),
	Fs = dig:get_nonempty_fs(D#dig.filters),

	?ASSERT(
		Fs /= [],
		"Please select at least one filter."
	),

	%
	% create
	%
	Context = wf_context:context(),
	Fun = fun({Fs1, Email}) ->
		wf_context:context(Context),
		handle_export_evaluator_stats_bulk(Fs1, Email)
	end,


	%
	% add to task queue
	%
	taskqueue:create(Fun, {Fs, itxauth:email()}),
	helper_ui:flash("Added to task queue. Please check email for zip file.").



handle_export_evaluator_stats_bulk(Fs, Email) ->

	%
	% init
	%
	dig:log(info, "Starting task ..."),
	Uid = helper:uidintstr(),
	Dir = "/tmp/" ++ Uid,


	%
	% create dir
	%
	helper:cmd("mkdir -p ~s", [Dir]),


	%
	% export in batches
	%
	done = handle_export_evaluator_stats_bulk(Fs, Dir, 0),
	handle_generate_xlsx(Dir),


	%
	% zip and mail dir
	%
	helper:zip_mail_clean_dir([Email], Dir, "OSM: Evaluator statistics export"),
	dig:log(success, "Task completed.").



handle_generate_xlsx(Dir) ->
	%
	% init
	%
	dig:log(warning, "Generating combined xlxs file ..."),
	SOffice = case os:cmd("uname -s") of
		"Darwin" ++ _ ->
			"/Applications/LibreOffice.app/Contents/MacOS/soffice";
		_ ->
			"soffice"
	end,


	%
	% combine all csv to single csv
	%
	helper:cmd("cd ~s; cat *.csv > combined.file; mv combined.file combined.csv", [
		Dir
	]),


	helper:cmd("cd ~s; ~s --headless --convert-to xlsx:'Calc MS Excel 2007 XML' combined.csv", [
		Dir, SOffice
	]).




handle_export_evaluator_stats_bulk(Fs, Dir, From) ->


	%
	% get docs in batches
	%
	dig:log(info, io_lib:format("Fetching docs from ~p", [From])),
	Docs = ep_osm_exam_api:fetch(From, ?BATCH_SIZE, Fs),



	%
	% create csv for every test
	%
	lists:foreach(fun(Doc) ->

		%
		% init
		%
		timer:sleep(1000),
		dig:log(warning, io_lib:format("Processing ... ~ts", [itf:val(Doc, testname)])),


		%
		% create dig for export
		%
		D = #dig {
			module=?MODULE,
			filters=[
				itf:build(itf:hidden(osm_exam_fk), itf:idval(Doc))
			]
		},


		%
		% create file
		%
		{_Name, FilePath} = handle_export_evaluator_stats_bulk_create_file(Doc, D),
		helper:cmd("mv ~s ~s", [FilePath, Dir]),
		dig:log(success, io_lib:format("Created ~s", [FilePath]))


	end, Docs),


	%
	% termination condiction
	%
	case length(Docs) < ?BATCH_SIZE of
		true ->
			done;
		_ ->
			handle_export_evaluator_stats_bulk(Fs, Dir, From + ?BATCH_SIZE)
	end.



handle_export_evaluator_stats_bulk_create_file(Doc, #dig {filters=Fs} = D) ->
	{Name, FilePath} = dig:get_filename_path(io_lib:format("~ts_~ts", [
		itf:val(Doc, anptestcourseid), dig:export_filename(D)
	])),
	dig:handle_export(Name, FilePath, D, Fs).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


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
% get class evaluator
%
get_class_evaluator(TotalEvaluators, _) when TotalEvaluators > 0 ->
	"table-success";
get_class_evaluator(0, TotalPapers) when TotalPapers > 0 ->
	"table-danger";
get_class_evaluator(_, _) ->
	[].



%
% get class
%
get_class({ok, Number}) when Number > 0 ->
	"table-info";
get_class(_) ->
	[].



%
% get val
%
get_val({ok, Val}) ->
	Val;
get_val(error) ->
	0.



%
% states
%
states(anpevaluator) -> [
	anpstate_not_uploaded,
	anpstate_yettostart,
	anpstate_active,
	anpstate_completed,
	anpstate_evaluation_rejected,
	anpstate_discarded
];

states(anpmoderator) -> [
	anpstate_moderation,
	anpstate_moderation_completed
];

states(anprevaluator) -> [
	anpstate_revaluation,
	anpstate_revaluation_completed
];

states(anpmoderator_reval) -> [
	anpstate_moderation_reval,
	anpstate_moderation_reval_completed
].




%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
