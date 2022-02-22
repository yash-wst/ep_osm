
-module(dig_ep_osm_exam_results).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("OSM Exam Results").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------

-define(BATCH_SIZE, 100).

-record(docs, {
	examdoc,
	seasondoc,
	programdoc,
	subjectdoc,
	mschemedoc,
	doc,
	rdsdoc,
	listofquestions=[],
	evaluatorrole,
	testtotalmarks
}).


%------------------------------------------------------------------------------
% ids
%------------------------------------------------------------------------------

exportids() -> [
	"season_code",
	"season_name",
	"program_code",
	"program_name",
	"subject_code",
	"subject_name",
	"testtotalmarks",
	"prn",
	"seatnumber",
	"evaluation_state",
	"booklet_number",
	"sticker_uid",
	"courseid",
	"profileidfk_anpevaluator",
	"anpevaluator_eval_date",
	"ip_anpevaluator",
	"evaluator_total",
	"profileidfk_anpmoderator",
	"anpmoderator_eval_date",
	"ip_anpmoderator",
	"moderator_total",
	"profileidfk_anprevaluator",
	"anprevaluator_eval_date",
	"ip_anprevaluator",
	"revaluator_total",
	"profileidfk_anpmoderator_reval",
	"anpmoderator_reval_eval_date",
	"ip_anpmoderator_reval",
	"moderator_reval_total",
	"total",
	"marks_per_question",
	"marks_per_marked_question"
].


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f("season_" ++ Id) ->
	F = ?COREXS(?L2A(Id)),
	F#field {label="Season " ++ F#field.label};

f("program_" ++ _ = Id) ->
	?CORPGM(?L2A(Id));

f("subject_" ++ _ = Id) ->
	?CORSUB(?L2A(Id));

f("prn") ->
	itf:textbox(?F(prn, "PRN"));

f("booklet_number") ->
	itf:textbox(?F(booklet_number, "Booklet Number"));

f("sticker_uid") ->
	itf:textbox(?F(sticker_uid, "Sticker UId"));

f("seatnumber") ->
	fields:get(anpseatnumber);

f("evaluation_state") ->
	fields:get(anpstate);

f("evaluator_total") ->
	fields:get(total_anpevaluator);

f("moderator_total") ->
	fields:get(total_anpmoderator);

f("revaluator_total") ->
	fields:get(total_anprevaluator);

f("moderator_reval_total") ->
	fields:get(total_anpmoderator_reval);

f("total") ->
	itf:textbox(?F(total, "Decided Total"));

f("courseid") ->
	fields:get(anptestcourseid);

f("marks_per_question") ->
	itf:textbox(?F(marks_per_question, "Marks Per Question"));

f("marks_per_marked_question") ->
	itf:textbox(?F(marks_per_marked_question, "Marks Per Marked Question"));

f("profileidfk_anpevaluator") ->
	fields:get(profileidfk_anpevaluator);

f("profileidfk_anpmoderator") ->
	fields:get(profileidfk_anpmoderator);

f("profileidfk_anpmoderator_reval") ->
	fields:get(profileidfk_anpmoderator_reval);

f("profileidfk_anprevaluator") ->
	fields:get(profileidfk_anprevaluator);

f("testtotalmarks") ->
	itf:textbox(?F(testtotalmarks, "Test Total Marks"));

f("ip_anpevaluator") ->
	itf:textbox(?F(ip_anpevaluator, "Evaluator IP"));

f("ip_anpmoderator") ->
	itf:textbox(?F(ip_anpmoderator, "Moderator IP"));

f("ip_anprevaluator") ->
	itf:textbox(?F(ip_anprevaluator, "Revaluator IP"));

f("ip_anpmoderator_reval") ->
	itf:textbox(?F(ip_anpmoderator_reval, "Moderator Reval IP"));


f(Id) ->
	fields:get(?L2A(Id)).

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, _) -> false.


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
	fields:get(startdate),
	itf:build(itf:hidden(osm_exam_fk), itxcontext:q(id))
].


%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=fs(search),
		size=25,
		actions=[
			{export_results_bulk, "Bulk Export Results", "Bulk Export Results"},
			{export_results_bulk_pdf, "Bulk Export Results (PDF)", "Bulk Export Results (PDF)"}
		],
		events=[
			ite:button(export, "CSV", {itx, {dig, export}}),
			ite:button(export_pdf, "PDF", {itx, {dig, export_pdf}})
		],
		instructions=[
			{ok, "You can add, remove and change the order of export by updating exportids shown below"},
			{ok, string:join(exportids(), ",\n")},
			{ok, #link {
				new=true,
				text="Change export format",
				url="/dig_config?keyid=ep_osm_result_exportids"
			}}
		],
		config=[
			{responsive_type, scroll},
			{show_slno, true}
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exam Results").



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
fetch(D, From, Size, [
]) ->
	fetch(D, From, Size, [fields:build(teststatus, ?COMPLETED)]);


%..............................................................................
%
% [osm_exam_fk]
%
%..............................................................................
fetch(D, From, Size, [
	#field {id=osm_exam_fk, uivalue=ExamId}
]) ->


	%
	% init
	%
	DefaultExportIds = string:join(exportids(), ","),
	ExportIds = string:tokens(
		itxconfigs:get2(ep_osm_result_exportids, DefaultExportIds), ","
	),
	ExportIds1 = lists:map(fun(Id) ->
		helper:trim(Id)
	end, ExportIds),
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),
	SeasonId = itf:val(ExamDoc, season_fk),
	ProgramId = itf:val(ExamDoc, program_code_fk),
	SubjectId = itf:val(ExamDoc, subject_code_fk),
	MSchemeId = itf:val(ExamDoc, osm_mscheme_fk),
	SeatNumberMappingId = itxconfigs_cache:get2(osm_images_folder_id, booklet_number),
	ListOfAllQuestions = get_list_of_questions(ExamDoc),
	TestTotalMarks = anptests:testtotalmarks(ExamDoc),


	%
	% get docs
	%
	SeasonDoc = itx:okdoc(ep_core_exam_season_api:get(SeasonId)),
	ProgramDoc = itx:okdoc(ep_core_program_api:get(ProgramId)),
	SubjectDoc = itx:okdoc(ep_core_subject_api:get(SubjectId)),
	MSchemeDoc = itx:okdoc(ep_osm_mscheme_api:get(MSchemeId)),


	%
	% get total count
	%
	Count = anpcandidates:getdocs_count(ExamId),


	%
	% get docs
	%
	Docs = anpcandidates:getdocs_from_to(ExamId, From, Size),


	%
	% get corresponding rds docs
	%
	SeatNumbers = lists:map(fun(Doc) ->
		itf:val(Doc, anpseatnumber)
	end, Docs),

	RdsDocs = case code:ensure_loaded(ep_rds_result_api) of
		{module, ep_rds_result_api} ->
			ep_rds_result_api:get_rds_docs(SeasonId, SubjectId, SeatNumbers, SeatNumberMappingId);
		_ ->
			[]
	end,
	RdsDocsDict = helper:get_dict_from_docs(RdsDocs, SeatNumberMappingId),



	%
	% layout results
	%
	Results = lists:map(fun(Doc) ->

		%
		% init
		%
		SeatNumber = itf:val(Doc, anpseatnumber),

		%
		% build docs record
		%
		RecDoc = #docs {
			examdoc=ExamDoc,
			seasondoc=SeasonDoc,
			programdoc=ProgramDoc,
			subjectdoc=SubjectDoc,
			mschemedoc=MSchemeDoc,
			doc=Doc,
			rdsdoc=dict:find(SeatNumber, RdsDocsDict),
			listofquestions=ListOfAllQuestions,
			evaluatorrole=get_evaluator_role(Doc),
			testtotalmarks=TestTotalMarks
		},


		%
		% vals
		%
		lists:foldl(fun
			(Id, Acc) when 
				Id == "marks_per_question";
				Id == "marks_per_marked_question" ->
				MPQVals = val(RecDoc, Id),
				Acc ++ lists:map(fun(MPQVal) ->
					#dcell {
						val=MPQVal
					}
				end, MPQVals);
			(Id, Acc) ->
				Acc ++ [
					#dcell {
						val=val(RecDoc, Id)
					}
				]
		end, [], ExportIds1)


	end, Docs),


	%
	% header
	%
	Header = lists:foldl(fun
		(Id, Acc) when 
			Id == "marks_per_question";
			Id == "marks_per_marked_question" ->
			Acc ++ get_question_headers(Id, MSchemeDoc, ListOfAllQuestions);
		(Id, Acc) ->
			#field {label=Label} = f(Id),
			Acc ++ [
				#dcell {
					type=header,
					val=Label
				}
			]
	end, [], ExportIds1),


	%
	% return
	%
	{
		D#dig {
			description=itl:render(itf:d2f_doc(ExamDoc, ?OSMEXM(osm_exam_fk))),
			total=Count,
			actions=[],
			dcell_headers=Header,
			config=D#dig.config ++ [
				{pdf_table_summary, [
					layout_pdf_header(ExamDoc, SeasonDoc, TestTotalMarks)
				]}
			]
		},
		Results
	};



%..............................................................................
%
% default
%
%..............................................................................

fetch(D, From, Size, Fs) ->


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
		ExamId = itf:idval(Doc),
		FsDoc = itf:d2f(Doc, anptest:fs(form)),

		%
		% sfps cells
		%
		SFPSCells = ep_core_dig_helper:get_sfps_cells(
			Doc, {SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict},
			#dcell {show_ui=false}
		),



		%
		% layout test
		%
		SFPSCells ++ lists:map(fun(F) ->
			#dcell {val=itl:render(F)}
		end, FsDoc) ++ [
			#dcell {
				show_csv=false,
				val=#link {
					new=true,
					url=io_lib:format("/~p?id=~s", [
						wf:page_module(), ExamId
					]),
					text="Results"
				}
			}
		]

	end, Docs),



	%
	% header
	%
	Header = [
		#dcell {type=header, show_ui=false, val="Season"},
		#dcell {type=header, show_ui=false, val="Faculty"},
		#dcell {type=header, show_ui=false, val="Program"},
		#dcell {type=header, show_ui=false, val="Subject"}
	] ++ lists:map(fun(#field {label=Label}) ->
		#dcell {type=header, val=Label}
	end, anptest:fs(form)) ++ [
		#dcell {type=header, show_csv=false, val="Actions"}

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



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event({itx, E}) ->
	ite:event(E);

event(export_results_bulk_pdf) ->
	handle_export_results_bulk_pdf();

event(export_results_bulk) ->
	handle_export_results_bulk().



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - bulk export results
%
%..............................................................................

handle_export_results_bulk() ->

	%
	% init
	%
	D = helper:state(dig),
	Fs = dig:get_nonempty_fs(D#dig.filters),


	%
	% assert
	%
	?ASSERT(
		Fs /= [],
		"Please select at least one filter."
	),


	%
	% process
	%
	case configs:getbool(process_via_minijob, false) of
		false ->
			handle_export_results_bulk_taskqueue();
		true ->
			handle_export_results_bulk_minijob()
	end.



%
% minijo
%
handle_export_results_bulk_minijob() ->
	Filters = itf:uivalue(fs(search)),
	Fs = dig:get_nonempty_fs(Filters),
	{ok, Doc} = minijob_osm_result_export:create_and_run(Fs),
	minijob_status:show_status(Doc).



%
% taskqueue
%
handle_export_results_bulk_taskqueue() ->

	%
	% init
	%
	D = helper:state(dig),
	Fs = dig:get_nonempty_fs(D#dig.filters),


	%
	% create
	%
	Context = wf_context:context(),
	Fun = fun({Fs1, Email}) ->
		wf_context:context(Context),
		handle_export_results_bulk(Fs1, Email)
	end,


	%
	% add to task queue
	%
	taskqueue:create(Fun, {Fs, itxauth:email()}),
	helper_ui:flash("Added to task queue. Please check email for zip file.").



handle_export_results_bulk(Fs, Email) ->

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
	done = handle_export_results_bulk(Fs, Dir, 0),


	%
	% zip and mail dir
	%
	helper:zip_mail_clean_dir([Email], Dir, "OSM: Results export"),
	dig:log(success, "Task completed.").



handle_export_results_bulk(Fs, Dir, From) ->


	%
	% get docs in batches
	%
	dig:log(info, io_lib:format("Fetching docs from ~p", [From])),
	?ASSERT(
		Fs /= [],
		"Please select at least one filter"
	),
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
			size=1000,
			filters=[
				itf:build(itf:hidden(osm_exam_fk), itf:idval(Doc))
			]
		},


		%
		% create file
		%
		{_Name, FilePath} = handle_export_results_bulk_create_file(Doc, D),
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
			handle_export_results_bulk(Fs, Dir, From + ?BATCH_SIZE)
	end.



handle_export_results_bulk_create_file(Doc, #dig {filters=Fs} = D) ->
	{Name, FilePath} = dig:get_filename_path(io_lib:format("~s_~s_~s", [
		itf:val(Doc, anptestcourseid), itf:val(Doc, testname), dig:export_filename(D)
	])),
	dig:handle_export(Name, FilePath, D, Fs).




%..............................................................................
%
% handle - bulk export results pdf
%
%..............................................................................

handle_export_results_bulk_pdf() ->

	%
	% init
	%
	D = helper:state(dig),
	Fs = dig:get_nonempty_fs(D#dig.filters),


	%
	% assert
	%
	?ASSERT(
		Fs /= [],
		"Please select at least one filter."
	),



	case configs:getbool(process_via_minijob, false) of
		false ->
			handle_export_results_bulk_pdf_taskqueue();
		true ->
			handle_export_results_bulk_pdf_minijob()
	end.



%
% minijo
%
handle_export_results_bulk_pdf_minijob() ->
	Filters = itf:uivalue(fs(search)),
	Fs = dig:get_nonempty_fs(Filters),
	{ok, Doc} = minijob_osm_result_export:create_and_run(Fs),
	minijob_status:show_status(Doc).



%
% taskqueue
%
handle_export_results_bulk_pdf_taskqueue() ->

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
		handle_export_results_bulk_pdf(Fs1, Email)
	end,


	%
	% add to task queue
	%
	taskqueue:create(Fun, {Fs, itxauth:email()}),
	helper_ui:flash("Added to task queue. Please check email for zip file.").



handle_export_results_bulk_pdf(Fs, Email) ->

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
	done = handle_export_results_bulk_pdf(Fs, Dir, 0),


	%
	% zip and mail dir
	%
	helper:zip_mail_link_clean_dir([Email], Dir, "OSM: PDF results export"),
	dig:log(success, "Task completed.").



handle_export_results_bulk_pdf(Fs, Dir, From) ->


	%
	% get docs in batches
	%
	dig:log(info, io_lib:format("Fetching docs from ~p", [From])),
	?ASSERT(
		Fs /= [],
		"Please select at least one filter"
	),
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

	
		SeasonId = itf:val(Doc, season_fk),
		TestTotalMarks = anptests:testtotalmarks(Doc),
		SeasonDoc = itx:okdoc(ep_core_exam_season_api:get(SeasonId)),

		%
		% create dig for export
		%
		D = #dig {
			module=?MODULE,
			size=1000,
			filters=[
				itf:build(itf:hidden(osm_exam_fk), itf:idval(Doc))
			],
			config=[
				{show_slno, true},
				{pdf_table_summary, [
					layout_pdf_header(Doc, SeasonDoc, TestTotalMarks)
				]}
			]
		},


		%
		% create file
		%
		{_Name, FilePath} = handle_export_results_bulk_pdf_create_file(Doc, D),
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
			handle_export_results_bulk_pdf(Fs, Dir, From + ?BATCH_SIZE)
	end.



handle_export_results_bulk_pdf_create_file(Doc, #dig {} = D) ->
	Filename = io_lib:format("~s_~s_~s", [
		itf:val(Doc, anptestcourseid), itf:val(Doc, testname), dig:export_filename(D)
	]),
	{Name, FilePath} = dig:get_filename_path(Filename, pdf),
	dig:handle_export_pdf(Name, FilePath, D).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


%
% vals
%
val(#docs {
	doc=Doc,
	evaluatorrole=EvaluatorRole,
	mschemedoc=MSchemeDoc
}, Id) when
	Id == "marks_per_marked_question" ->
	get_marks_per_marked_question(
		MSchemeDoc, Doc, EvaluatorRole
	);



val(#docs {
	doc=Doc,
	listofquestions=ListOfAllQuestions,
	evaluatorrole=EvaluatorRole
}, Id) when
	Id == "marks_per_question" ->
	get_marks_per_question(Doc, ListOfAllQuestions, EvaluatorRole);



val(#docs {
	examdoc=ExamDoc
}, Id) when
	Id == "courseid" ->
	itf:val(ExamDoc, f(Id));


val(#docs {
	testtotalmarks=TestTotalMarks
}, Id) when
	Id == "testtotalmarks" ->
	TestTotalMarks;


val(#docs {
	seasondoc=SeasonDoc
}, Id) when
	Id == "season_name";
	Id == "season_code" ->
	itf:val(SeasonDoc, f(Id));



val(#docs {
	programdoc=ProgramDoc
}, Id) when
	Id == "program_name";
	Id == "program_code" ->
	itf:val(ProgramDoc, f(Id));



val(#docs {
	subjectdoc=SubjectDoc
}, Id) when
	Id == "subject_name";
	Id == "subject_code" ->
	itf:val(SubjectDoc, f(Id));



val(#docs {
	doc=Doc
}, Id) when
	Id == "evaluator_total" ->
	Val = itf:val(Doc, f(Id)),
	AnpState = itf:val(Doc, anpstate),
	case {Val, AnpState} of
		{_, "anpstate_discarded"} ->
			"Discarded";
		{_, "anpstate_not_uploaded"} ->
			"Absent";
		{_, "anpstate_active"} ->
			"Active";
		{[], _} ->
			[];
		_ -> helper:i2s(helper:ceiling(helper:s2f_v1(Val)))
	end;
val(#docs {
	doc=Doc
}, Id) when
	Id == "moderator_total";
	Id == "revaluator_total";
	Id == "moderator_reval_total" ->
	Val = itf:val(Doc, f(Id)),
	case Val of
		[] ->
			[];
		_ -> helper:i2s(helper:ceiling(helper:s2f_v1(Val)))
	end;



val(#docs {
	rdsdoc={ok, RdsDoc}
}, Id) when
	Id == "prn";
	Id == "booklet_number";
	Id == "sticker_uid" ->
	itf:val(RdsDoc, f(Id));



val(#docs {
	rdsdoc=error
}, Id) when
	Id == "prn";
	Id == "booklet_number";
	Id == "sticker_uid" ->
	[];


val(#docs {
	doc=Doc
}, Id) when
	Id == "profileidfk_anpevaluator";
	Id == "profileidfk_anpmoderator";
	Id == "profileidfk_anprevaluator";
	Id == "profileidfk_anpmoderator_reval" ->

	ProfileId = itf:val(Doc, f(Id)),
	Fun = fun() ->
		profiles:getdoc(ProfileId)
	end,
	case ProfileId of
		[] ->
			[];
		_ ->
			{ok, ProfileDoc} = itxdoc_cache:get({?MODULE, ProfileId}, Fun),
			profiles:displayname_fmt(helper_api:doc2fields({ok, ProfileDoc}))
	end;


val(#docs {
	doc=Doc
}, "evaluation_state" = Id) ->
	?LN(?L2A(itf:val(Doc, f(Id))));

val(#docs {
	evaluatorrole=Role
} = RecDoc, "total" = Id) ->
	val(RecDoc, ?FLATTEN(Role ++ "_" ++ Id));



val(#docs {
	doc=Doc
}, "ip_" ++ Role) ->
	try
		get_ipaddress(Role, Doc)
	catch E:M ->
		?D({E, M, erlang:get_stacktrace()}),
		[]
	end;


val(#docs {
	doc=Doc
}, Id) ->
	itf:val(Doc, f(Id)).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% get - marks per marked question
%
get_marks_per_marked_question(MSchemeDoc, Doc, EvaluatorRole) ->
	%
	% init
	%
	EvaluatorMarkingId = ?L2A(?FLATTEN(io_lib:format("anpmarking_anp~s", [EvaluatorRole]))),
	List = ep_osm_mscheme:handle_get_marks_per_question(MSchemeDoc, Doc, EvaluatorMarkingId),
	MarkedQuestions = itf:val(MSchemeDoc, ?OSMMSC(list_of_export_markers)),

	%
	% lists map
	%
	lists:map(fun({Id, "on"}) ->
		IdAtom = ?L2A(Id),
		case lists:keyfind(IdAtom, 1, List) of
			{IdAtom, _QName, Marks, _MaxMarks} ->
				helper:f2s_v1(Marks);
			false ->
				"error"
		end
	end, MarkedQuestions).


%
% get - marks per question
%
get_marks_per_question(Doc, ListOfAllQuestions, EvaluatorRole) ->
	%
	% init
	%
	EvaluatorMarkingId = ?L2A(?FLATTEN(io_lib:format("anpmarking_anp~s", [EvaluatorRole]))),
	MarkingValues = itf:val(Doc, fields:get(EvaluatorMarkingId)),
	MarkingValuesDict = dict:from_list(MarkingValues),

	%
	% get values
	%
	lists:foldl(fun({MarkingId, _QuestionId, _MaxMarks}, Acc) ->
		Val = case dict:find(MarkingId, MarkingValuesDict) of
			{ok, ObtainedMarksFloatStr} ->
				ObtainedMarks = helper:s2f_v1(ObtainedMarksFloatStr),
				lists:flatten(io_lib:format("~.2f", [ObtainedMarks]));
			error ->
				[]
		end,
		Acc ++ [Val]
	end, [], ListOfAllQuestions).



%
% get - list of questions
%

get_list_of_questions(TDoc) ->
	TFs = helper_api:doc2fields({ok, TDoc}),
	anp_marking:init_marking_rules(TFs),
	ListofRules = anp_marking:get_marking_rules(),
	lists:flatten(anp_marking:getquestion_marks_rules(ListofRules)).



%
% get - question headers
%
get_question_headers("marks_per_marked_question", MSchemeDoc, _ListOfAllQuestions) ->

	EvaluatorMarkingId = anpmarking_anpevaluator,
	List = ep_osm_mscheme:handle_get_marks_per_question(MSchemeDoc, {[]}, EvaluatorMarkingId),
	MarkedQuestions = itf:val(MSchemeDoc, ?OSMMSC(list_of_export_markers)),

	%
	% lists map
	%
	lists:map(fun({Id, "on"}) ->
		IdAtom = ?L2A(Id),
		{QuestionLabel, MaxMarksLabel} = case lists:keyfind(IdAtom, 1, List) of
			{_IdAtom, QName, _Marks, MaxMarks} ->
				{QName, itx:format("~.2f", [MaxMarks])};
			false ->
				{"error", "error"}
		end,
		#dcell {
			type=header,
			val=[
				#p {style="margin: 0px;", text=QuestionLabel},
				#p {style="margin: 0px;", text=MaxMarksLabel}
			],
			val_export=string:join([QuestionLabel, MaxMarksLabel], " / ")
		}
	end, MarkedQuestions);


get_question_headers("marks_per_question", _, ListOfAllQuestions) ->
	lists:map(fun({_MarkingId, QuestionId, MaxMarks}) ->
		#dcell {
			type=header,
			val=[
				#p {style="margin: 0px;", text=QuestionId},
				#p {style="margin: 0px;", text=helper:n2s(MaxMarks)}
			],
			val_export=string:join([QuestionId, helper:n2s(MaxMarks)], " / ")
		}
	end, ListOfAllQuestions).



%
% get evaluator role
%
get_evaluator_role(Doc) ->
	case {
		itf:val(Doc, total_anpmoderator_reval),
		itf:val(Doc, total_anprevaluator),
		itf:val(Doc, total_anpmoderator),
		itf:val(Doc, total_anpevaluator)
	} of
		{X, _, _, _} when X /= [] ->
			"moderator_reval";
		{_, X, _, _} when X /= [] ->
			"revaluator";
		{_, _, X, _} when X /= [] ->
			"moderator";
		_ ->
			"evaluator"
	end.



%
% get ip address of
%
get_ipaddress(Role, Doc) ->
	ProfileFId = ?L2A("profileidfk_" ++ Role),
	ProfileId = itf:val(Doc, ProfileFId),
	Fun = fun() ->
		profiles:getdoc(ProfileId)
	end,
	case ProfileId of
		[] ->
			[];
		_ ->
			{ok, ProfileDoc} = itxdoc_cache:get({?MODULE, ProfileId}, Fun),
			Username = itf:val(ProfileDoc, username),
			Comments = itf:val(Doc, fields:get(comments)),
			get_ipaddress_from_username(Username, Comments)
	end.


get_ipaddress_from_username(Username, Comments) ->
	lists:foldl(fun({Key, _Val}, Acc) ->
		[_Date, _Time, IP, User] = string:tokens(Key, " "),
		case Username == User of
			true ->
				IP;
			_ ->
				Acc
		end
	end, [], Comments).




%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------

layout_pdf_header(ExamDoc, SeasonDoc, TestTotalMarks) ->


	%
	% init
	%
	TestId = itf:idval(ExamDoc),
	Stats = ep_osm_exam_api:get_evaluation_stats0(TestId),


	%
	% total absent
	%
	TotalAbsent = proplists:get_value(["anpstate_not_uploaded"], Stats, 0),
	TotalDiscarded = proplists:get_value(["anpstate_discarded"], Stats, 0),
	TotalPresent = lists:foldl(fun(AnpState, Acc) ->
		Count = proplists:get_value([?A2L(AnpState)], Stats, 0),
		case AnpState of
			anpstate_discarded ->
				Acc;
			anpstate_not_uploaded ->
				Acc;
			_ ->
				Acc + Count
		end
	end, 0, helper_options:options(anpstate)),



	%
	% display
	%
	#table {
		class="table table-bordered table-sm",
		rows=[
			#tablerow {cells=[
				#tablecell {
					colspan=2,
					body=#p {
						style="margin: 0px",
						class="text-uppercase mycenter",
						text=itf:val(SeasonDoc, name)
					}
				}
			]},
			#tablerow {cells=[
				#tablecell {style="width: 25%", body="Exam Code"},
				#tablecell {body=itf:val(ExamDoc, anptestcourseid)}
			]},
			#tablerow {cells=[
				#tablecell {body="Exam Name"},
				#tablecell {body=itf:val(ExamDoc, testname)}
			]},
			#tablerow {cells=[
				#tablecell {body="Total Marks"},
				#tablecell {body=TestTotalMarks}
			]},
			#tablerow {cells=[
				#tablecell {body="Total Present"},
				#tablecell {text=TotalPresent}
			]},
			#tablerow {cells=[
				#tablecell {body="Total Absent"},
				#tablecell {text=TotalAbsent}
			]},
			#tablerow {cells=[
				#tablecell {body="Total Discarded"},
				#tablecell {text=TotalDiscarded}
			]}
	]}.

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
