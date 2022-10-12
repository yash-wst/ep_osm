
-module(dig_ep_osm_exam_evaluator_report).
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
	?LN("OSM Evaluator Report").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------

-define(BATCH_SIZE, 100).


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, Role) when
	Role == ?APPOSM_EVALUATOR;
	Role == ?APPOSM_MODERATOR;
	Role == ?APPOSM_REVALUATOR;
	Role == ?APPOSM_MODERATOR_REVAL ->
		true;
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, ?APPOSM_CONTROLLER) -> true;
access(_, ?APPOSM_CAPADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	RoleGroup = get_role_group(itxauth:role()),
	#dig {
		module=?MODULE,
		pdf_orientation=portrait,
		filters=get_filters(RoleGroup),
		show_filter=get_show_filter(RoleGroup),
		size=25,
		actions=get_actions(RoleGroup),
		events=get_events(RoleGroup),
		config=[
			{responsive_type, scroll},
			{show_slno, true},
			{pdf_table_summary, layout_table_summary()},
			{pdf_table_footer, layout_table_footer()}
		]
	}.


%
% get - events
%
get_events(regular) -> [
];
get_events(evaluator) -> [
];
get_events(admin) -> [
	ite:button(export, "CSV", {itx, {dig, export}})
].



%
% get - actions
%
get_actions(regular) -> [
];
get_actions(evaluator) -> [
];
get_actions(admin) -> [
	{export_evaluator_stats_bulk, "Bulk Export Evaluator Report", "Bulk Export Evaluator Report"}
].


%
% get - show filters
%
get_show_filter(regular) ->
	false;
get_show_filter(evaluator) ->
	false;
get_show_filter(admin) ->
	true.



%
% get - filters
%
get_filters(regular) -> [
	itf:build(itf:hidden(osm_exam_fk), itxcontext:q(id)),
	itf:build(itf:textbox(?F(profileid)), itxcontext:q(evaluatorid))
];
get_filters(evaluator) -> [
	itf:build(itf:textbox(?F(role)), itxauth:role()),
	itf:build(itf:textbox(?F(profileid)), itxauth:profileid())
];
get_filters(admin) -> [
	?COREXS(season_fk),
	?CORFAC(faculty_code_fk),
	?CORPGM(program_code_fk),
	?CORSUB(subject_code_fk),
	fields:get(anptestcourseid),
	fields:get(teststatus),
	fields:get(exam_pattern),
	itf:build(itf:hidden(osm_exam_fk), itxcontext:q(id)),
	itf:build(itf:hidden(profileid), itxcontext:q(evaluatorid))
].


%
% get role group
%
get_role_group(Role) ->
	if
		Role == ?APPOSM_EVALUATOR;
		Role == ?APPOSM_MODERATOR;
		Role == ?APPOSM_REVALUATOR;
		Role == ?APPOSM_MODERATOR_REVAL -> evaluator;
		Role == ?APPOSM_ADMIN;
		Role == ?APPOSM_ANPADMIN;
		Role == ?APPOSM_CONTROLLER -> admin;
		Role == ?APPOSM_CAPADMIN -> regular;
		true -> undefined
	end.



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
fetch(D, _From, _Size, [
	#field {id=role},
	#field {id=profileid, uivalue=EvaluatorId}
]) ->

	%
	% init
	%
	ProfileFs = profiles:get(EvaluatorId),
	Tests = index_anpevaluator:get_eligible_tests(
		ProfileFs, index_anpevaluator:get_tests_from_cache()
	),


	%
	% layout tests
	%
	Results = lists:map(fun(TDoc) ->

		%
		% init
		%
		SeasonName = case itf:val(TDoc, season_fk) of
			[] ->
				[];
			SeasonId ->
				SeasonDoc = ep_core_exam_season_api:get_from_cache(SeasonId),
				itf:val(SeasonDoc, name)
		end,


		%
		% cells
		%
		[
			#dcell {val=SeasonName},
			#dcell {val=itf:val(TDoc, anptestcourseid)},
			#dcell {val=itf:val(TDoc, testname)},
			#dcell {val=#button {
				text="Download",
				class="btn btn-primary-outline",
				postback={download, itf:idval(TDoc), EvaluatorId},
				delegate=?MODULE,
				actions=#event {
					type=click,
					actions=#add_class {class="disabled"}
				}
			}}
		]

	end, Tests),



	%
	% header
	%
	Header = [
		#dcell {type=header, val="Exam Season"},
		#dcell {type=header, val="Exam Code"},
		#dcell {type=header, val="Exam Name"},
		#dcell {type=header, val="Download"}
	],


	{
		D#dig {
			description="Evaluation Reports",
			total=length(Tests)
		},
		[Header | Results]
	};


%..............................................................................
%
% [osm_exam_fk]
%
%..............................................................................
fetch(D, _From, _Size, [
	#field {id=osm_exam_fk, uivalue=ExamId},
	#field {id=profileid, uivalue=EvaluatorId}
]) ->

	%
	% get stats
	%
	Stats = ep_osm_exam_api:get_capcentre_stats_test(ExamId, all, 4),
	Stats1 = lists:filter(fun({[_IP, _State, _Date, ProfileId], _Count}) ->
		EvaluatorId == ProfileId
	end, Stats),
	Stats2 = lists:sort(fun(A, B) ->
		A < B
	end, Stats1),


	%
	% get unique ips
	%
	IPs = lists:map(fun({[IP, _State, _Date, _ProfileId], _Count}) ->
		IP
	end, Stats2),
	IPs1 = helper:sort(helper:unique(IPs)),


	%
	% get cap centre dict
	%
	CapCentreDocsDict = ep_osm_cap_api:getdocs_dict_by_ip(IPs1),


	%
	% layout results
	%
	{Results, TotalCount} = lists:foldl(fun(
			{[IP, _State, Date, _EvaluatorId], Count}, {Acc, AccCount}
	) ->
		CapCentreName = case dict:find(IP, CapCentreDocsDict) of
			{ok, CapDoc} ->
				itf:val(CapDoc, name);
			_ ->
				""
		end,
		{
			Acc ++ [[
				#dcell {val=Date},
				#dcell {val=CapCentreName},
				#dcell {val=IP},
				#dcell {val=Count}
			]],
			AccCount + Count
		}
	end, {[], 0}, Stats2),



	%
	% Header
	%
	Header = [
		#dcell {width=26, type=header, val="Date"},
		#dcell {type=header, val="CAP Centre Name"},
		#dcell {type=header, val="IP"},
		#dcell {type=header, val="Count"}
	],


	%
	% return
	%
	{
		D#dig {
			show_filter=false,
			total=length(Results),
			description="Evaluator Report",
			events=[
				#button {
					class="btn btn-primary-outline",
					text="Download",
					postback=export_pdf,
					delegate=?MODULE,
					actions=#event {
						type=click,
						actions=#add_class {class="disabled"}
					}
				}
			],
			actions=[],
			dcell_headers=Header
		},
		Results ++ [[
			#dcell {val=#p {class="font-weight-bold", text="Total"}},
			#dcell {val=#p {class="font-weight-bold", text=""}},
			#dcell {val=#p {class="font-weight-bold", text=""}},
			#dcell {val=#p {class="font-weight-bold", text=TotalCount}}
		]]
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
	Stats = ep_osm_exam_api:get_capcentre_stats_test(ExamId, all, 4),
	Stats1 = lists:sort(fun(A, B) ->
		A < B
	end, Stats),


	%
	% get unique ips
	%
	IPs = lists:map(fun({[IP, _State, _Date, _ProfileId], _Count}) ->
		IP
	end, Stats1),
	IPs1 = helper:sort(helper:unique(IPs)),


	%
	% get cap centre dict
	%
	CapCentreDocsDict = ep_osm_cap_api:getdocs_dict_by_ip(IPs1),


	%
	% get profile docs
	%
	ProfileIds = lists:map(fun({[_IP, _State, _Date, ProfileId], _Count}) ->
		ProfileId
	end, Stats1),
	ProfileIdsUnique = helper:unique(ProfileIds),
	ProfileDocs = profiles:getdocs_by_ids(ProfileIdsUnique),
	ProfileDocsDict = helper:get_dict_from_docs(ProfileDocs),



	%
	% layout results
	%
	Results = lists:map(fun({[IP, _State, Date, EvaluatorId], Count}) ->

		%
		% init
		%
		ProfileDoc = helper:get_doc_or_empty_doc_from_dict(EvaluatorId, ProfileDocsDict),

		CapCentreName = case dict:find(IP, CapCentreDocsDict) of
			{ok, CapDoc} ->
				itf:val(CapDoc, name);
			_ ->
				""
		end,

		%
		% get stats per profile
		%
		SFPSCells ++ [
			#dcell {
				val=itf:val(ProfileDoc, username)
			},
			#dcell {
				val=itf:val(ProfileDoc, fullname)
			},
			#dcell {
				type=label,
				val=itf:val(ProfileDoc, mobile)
			},
			#dcell {
				val=itf:val(ProfileDoc, email)
			},
			#dcell {
				val=?LN(?L2A(itf:val(ProfileDoc, profiletype)))
			},
			#dcell {
				val=Date
			},
			#dcell {
				val=IP
			},
			#dcell {
				val=CapCentreName
			},
			#dcell {
				val=Count
			},
			#dcell {
				val= itf:val(ProfileDoc, beneficiary_name)
			},
			#dcell {
				val= itf:val(ProfileDoc, bank_name)
			},
			#dcell {
				val= itf:val(ProfileDoc, bank_branch_name)
			},
			#dcell {
				val= itf:val(ProfileDoc, bank_account_no)
			},
			#dcell {
				val= itf:val(ProfileDoc, bank_ifsc_code)
			},
			#dcell {
				val= itl:render(ProfileDoc, fields:get(bank_account_type))
			},
			#dcell {
				val= itf:val(ProfileDoc, pancard_n)
			},
			#dcell {
				show_csv=false,
				val=#link {
					new=true,
					text="Report",
					url=itx:format("/~p?id=~s&evaluatorid=~s", [
						?MODULE, ExamId, EvaluatorId
					])
				}
			}
		]

	end, Stats1),



	%
	% header
	%
	Header = ep_core_dig_helper:get_sfps_cells_header() ++ [
		#dcell {type=header, val="Username"},
		#dcell {type=header, val="Fullname"},
		#dcell {type=header, val="Mobile"},
		#dcell {type=header, val="Email"},
		#dcell {type=header, val="Role"},
		#dcell {type=header, val="Date"},
		#dcell {type=header, val="IP"},
		#dcell {type=header, val="CAP Centre Name"},
		#dcell {type=header, val="Count"},
		#dcell {type=header, val="Beneficiary_name"},
		#dcell {type=header, val="Bank Name"},
		#dcell {type=header, val="Branch Name"},
		#dcell {type=header, val="Account Number"},
		#dcell {type=header, val="IFSC code"},
		#dcell {type=header, val="Account Type"},
		#dcell {type=header, val="PAN Card No"},
		#dcell {type=header, show_csv=false, val="Report"}
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
				text=io_lib:format("~s / ~s / ~s", [
					itf:val(TFs, anptestcourseid),
					itf:val(TFs, testname),
					?LN(?L2A(itf:val(TFs, teststatus)))
				])
			}
		},
		[Header] ++ ResultsSorted
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
			dcell_exam_actions(Doc)
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



%..............................................................................
%
% layout - table summary
%
%..............................................................................

layout_table_summary() ->
	layout_table_summary(itxcontext:q(id), itxcontext:q(evaluatorid)).

layout_table_summary(ExamId, EvaluatorId) when
	ExamId /= undefined, EvaluatorId /= undefined ->
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),
	FsReport = lists:map(fun(F) ->
		F#field {validators=[]}
	end, anptest:fs(report)),
	Es = itl:get(?VIEW, itf:d2f(ExamDoc, FsReport), noevent, tableonly),
	[
		#p {text="Exam Details", class="font-weight-bold"},
		Es
	];

layout_table_summary(_, _) ->
	undefined.



%..............................................................................
%
% layout - table footer
%
%..............................................................................

layout_table_footer() ->
	layout_table_footer(itxcontext:q(id), itxcontext:q(evaluatorid)).

layout_table_footer(ExamId, EvaluatorId) when
	ExamId /= undefined, EvaluatorId /= undefined ->

	{ok, ProfileDoc} = profiles:getdoc(EvaluatorId),
	FsAck = lists:map(fun(F) ->
		F#field {validators=[]}
	end, profile_anpevaluator:fs(ack)),
	Es = itl:get(?VIEW, itf:d2f(ProfileDoc, FsAck),
		noevent, tableonly),
	[
		#p {text="Examiner Details", class="font-weight-bold"},
		Es
	];

layout_table_footer(_, _) ->
	undefined.


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({download, ExamId, ProfileId}) ->
	handle_print_evaluator_report(ExamId, ProfileId);

event(export_pdf) ->
	handle_print_evaluator_report(wf:q(id), wf:q(evaluatorid));

event(export_evaluator_stats_bulk) ->
	handle_export_evaluator_stats_bulk();

event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------

%..............................................................................
%
% handle - print evaluator report
%
%..............................................................................

handle_print_evaluator_report(ExamId, EvaluatorId) ->
	%
	% init
	%
	Dig= #dig {
		module=?MODULE,
		size=1000,
		filters=[
			itf:build(itf:hidden(?F(osm_exam_fk)), ExamId),
			itf:build(itf:hidden(?F(profileid)), EvaluatorId)
		],
		pdf_orientation=portrait,
		config=[
			{responsive_type, scroll},
			{show_slno, true},
			{pdf_table_summary, layout_table_summary(ExamId, EvaluatorId)},
			{pdf_table_footer, layout_table_footer(ExamId, EvaluatorId)}
		]
	},
	Filename = itx:format("~s_~s", [
		ExamId, EvaluatorId
	]),
	{Name, FilePath} = dig:get_filename_path(Filename, pdf),
	dig:handle_export_pdf(Name, FilePath, Dig),
	itxdownload:stream(Name, FilePath).




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
	helper:zip_mail_clean_dir([Email], Dir, "OSM: Evaluator report export"),
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
		dig:log(warning, io_lib:format("Processing ... ~s", [itf:val(Doc, testname)])),


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
	{Name, FilePath} = dig:get_filename_path(io_lib:format("~s_~s_~s", [
		itf:val(Doc, anptestcourseid), itf:val(Doc, testname), dig:export_filename(D)
	])),
	dig:handle_export(Name, FilePath, D, Fs).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



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



%
% exam actions
%
dcell_exam_actions(Doc) ->
	#dcell {
		val_export="",
		val=#link {
			text="View",
			url=itx:format("~p?id=~s", [
				?MODULE, itf:idval(Doc)
			])
		}
	}.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
