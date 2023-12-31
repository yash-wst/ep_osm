
-module(dig_mm_ep_osm_exam_from_frp).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-import(minijob_import_from_rps, [f/1]).


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("ANP Test Import - From Result Processing System").

heading() ->
	title().

form() ->
	ep_osm_exam.

module(import) ->
	ep_osm_exam_import_from_frp.

%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fs
%------------------------------------------------------------------------------

fs(import) -> [
	f(date_of_test),
	itf:build(f(auto_create_bundles), ?NO)
];

fs(form) ->
	Ids = [
		season_fk,
		anptestcourseid,
		exam_pattern,
		teststatus,
		testtotalmarks,
		testduration,
		startdate,
		osm_mscheme_fk
	],
	fields:getfields(Ids).

%------------------------------------------------------------------------------
% fs - group
%------------------------------------------------------------------------------

fields(_, _) ->
	fs(form).



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		mode=?VIEW,
		module=?MODULE,
		filters=fs(form),
		size=25,
		actions=[
			{action_import, "Create by date", "Create by date"}
		],
		config=[
			{actions_default, false},
			{action_layout_type, buttons}
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("ANP Test Import - From Result Processing System").



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
fetch(D, From, Size, Fs) ->
	dig_mm:fetch(D, From, Size, Fs).



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	dig_mm:layout(?MODULE).



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(import_from_frp) ->
	handle_import_from_frp();

event(action_import) ->
	handle_action_import();

event(E) ->
	dig_mm:event(E).

start_upload_event(Event) ->
	dig_mm:start_upload_event(Event).

finish_upload_event(Tag, AttachmentName, LocalFileData, Node) ->
	dig_mm:finish_upload_event(Tag, AttachmentName, LocalFileData, Node).

%------------------------------------------------------------------------------
% assertions
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% save
%------------------------------------------------------------------------------


%
% override before save function
%
before_save(FsToSave, _FsAll, _Doc) ->
	FsToSave.


%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - import from frp
%
%..............................................................................

handle_import_from_frp() ->
	case configs:getbool(process_via_minijob, false) of
		false ->
			handle_import_from_frp_via_taskqueue();
		true ->
			handle_import_from_frp_via_minijob()
	end.


handle_import_from_frp_via_minijob() ->
	Fs = itf:uivalue(fs(import)),
	{ok, Doc} = minijob_import_from_rps:create_and_run(Fs),
	minijob_status:show_status(Doc).


handle_import_from_frp_via_taskqueue() ->
	%
	% init
	%
	Context = wf_context:context(),
	DateOfExam = wf:q(date_of_test),
	AutoCreateBundles = wf:q(auto_create_bundles),
	Email = myauth:email(),

	%
	% function
	%
	Fun = fun([]) ->
		wf_context:context(Context),
		handle_import_from_frp(DateOfExam, AutoCreateBundles),
		email:send(
			[Email],
			"Create exams from RPS",
			io_lib:format("Task completed. Date of exam : ~s", [DateOfExam])
		)
	end,


	%
	% add to queue
	%
	taskqueue:create(Fun, []),
	helper_ui:flash(warning, "Added to queue.", 5).


handle_import_from_frp(DateOfExam, AutoCreateBundles) ->

	%
	% init
	%
	FDate = itf:build(itf:date(?F(date)), DateOfExam),

	%
	% get exam from frp of matching date
	%
	{_, FrpExamDocs} = rpc_call(
		itxnode:frp(),
		up_core_exam_api,
		get_by_field,
		[FDate]
	),


	ActiveFrpExamDocs = lists:filter(fun(EDoc) ->
		itf:val2(EDoc, state) == ?ACTIVE
	end, FrpExamDocs),

	dig:log(info, io_lib:format("Found ~p exam docs on FRP system", [length(ActiveFrpExamDocs)])),


	%
	% for each doc
	%
	lists:foreach(fun(FrpExamDoc) ->
		handle_import_from_frp_examdoc(DateOfExam, AutoCreateBundles, FrpExamDoc)
	end, ActiveFrpExamDocs),


	%
	% finish
	%
	dig:log(info, "Task completed").




%..............................................................................
%
% handle - import from frp exam doc
%
%..............................................................................

handle_import_from_frp_examdoc(DateOfExam, AutoCreateBundles, FrpExamDoc) ->

	%
	% init
	%
	SeasonId = itf:val(FrpExamDoc, season_fk),
	SubjectId = itf:val(FrpExamDoc, subject_code_fk),


	%
	% get season and subject docs from frp
	%
	{ok, FrpSeasonDoc} = rpc_call(
		itxnode:frp(),
		ep_core_exam_season_api,
		get,
		[SeasonId]
	),
	{ok, FrpSubjectDoc} = rpc_call(
		itxnode:frp(),
		ep_core_subject_api,
		get,
		[SubjectId]
	),


	dig:log(warning, "PROCESSING ... " ++ itf:val(FrpSubjectDoc, subject_code) ++ " / " ++ itf:val(FrpSubjectDoc, pattern)),


	%
	% check if season doc exists else, create it
	%
	{ok, OsmSeasonDoc} = handle_import_from_frp_examdoc_ensure_season_exists(
		FrpSeasonDoc
	),


	%
	% check if subject doc exists else, create it
	%
	{ok, _OsmSubjectDoc} = handle_import_from_frp_examdoc_ensure_subject_exists(
		FrpSubjectDoc
	),


	%
	% check if exam doc exists else, create it
	%
	ResOsmExamDoc = handle_import_from_frp_examdoc_ensure_examdoc_exists(
		DateOfExam, OsmSeasonDoc, FrpSubjectDoc, FrpExamDoc
	),


	%
	% upload or update student list
	%
	handle_import_from_frp_examdoc_upload_student_list(
		FrpExamDoc, AutoCreateBundles, ResOsmExamDoc
	).



%..............................................................................
%
% handle - upload student list
%
%..............................................................................

handle_import_from_frp_examdoc_upload_student_list(FrpExamDoc, AutoCreateBundles, {ok, OsmExamDoc}) ->


	%
	% init
	%
	dig:log(info, "Updating student list ... "),
	FrpSeasonId = itf:val(FrpExamDoc, season_fk),
	FrpSubjectId = itf:val(FrpExamDoc, subject_code_fk),
	MarkTypeId = get_mark_type(OsmExamDoc),


	%
	% get student list from frp
	%
	FrpStudentList0 = rpc_call(
		itxnode:frp(),
		dig_result_upload_handlers,
		handle_download_prns,
		[FrpSeasonId, FrpSubjectId, MarkTypeId]
	),
	FrpStudentList = sanitise_frp_list(FrpStudentList0),
	save_frp_list_to_file(FrpStudentList),
	LoLFrpStudentList = helper:list_split(FrpStudentList, 250),


	%
	% start import
	%
	lists:foreach(fun(FrpStudentListBatch) ->
		handle_import_from_frp_examdoc_upload_student_list_batch(
			FrpExamDoc, OsmExamDoc, FrpStudentListBatch, AutoCreateBundles
		)
	end, LoLFrpStudentList),

	dig:log(success, io_lib:format("Completed for: ~ts", [
		itf:val(OsmExamDoc, anptestcourseid)
	]));



handle_import_from_frp_examdoc_upload_student_list(_, _, _) ->
	skip.


%..............................................................................
%
% handle - upload student list batch
%
%..............................................................................

handle_import_from_frp_examdoc_upload_student_list_batch(
	FrpExamDoc, OsmExamDoc, FrpStudentList, AutoCreateBundles
) ->


	%
	% init
	%
	dig:log(info, "Updating student list batch ... "),
	OsmExamId = itf:idval(OsmExamDoc),
	ExamDb = anpcandidates:db(OsmExamId),
	FrpSeasonId = itf:val(FrpExamDoc, season_fk),
	FrpSubjectId = itf:val(FrpExamDoc, subject_code_fk),
	PRNs = get_prns_from_frp_list(FrpStudentList),
	SeatNumberId = ep_osm_id:get(anpseatnumber, in, profile_student),
	BundleId = create_bundle(AutoCreateBundles, FrpSeasonId, OsmExamId),
	EpochTime = helper:epochtime(),


	%
	% create student profile docs dict
	%
	ProfileDocsDict = get_profile_docs_dict_from_frp_list(
		FrpStudentList, SeatNumberId
	),


	%
	% prns may not be seat numbers so get the proper value based on
	% seatnumber id
	%
	SeatNumbers = lists:map(fun(PRN) ->
		import_anpseatnumber(PRN, dict:find(PRN, ProfileDocsDict), SeatNumberId)
	end, PRNs),


	%
	% get candidates docs from osm exam db and create dict
	%
	FsFind = [
		db2es_find:get_field_cond("$in", anpseatnumber, SeatNumbers)
	],
	OsmCandidateDocs = ep_osm_candidate_api:fetch(OsmExamId, 0, ?INFINITY, FsFind, [
		{use_index, ["anpseatnumber"]}
	]),
	OsmCandidateDocsDict = helper:get_dict_from_docs(OsmCandidateDocs, anpseatnumber),


	%
	% find student list that does not exist on osm
	%
	FrpStudentListMissing = lists:filter(fun([PRN | _]) ->
		SeatNumber = import_anpseatnumber(PRN, dict:find(PRN, ProfileDocsDict), SeatNumberId),
		dict:find(SeatNumber, OsmCandidateDocsDict) == error
	end, FrpStudentList),



	%
	% logs
	%
	dig:log(info, io_lib:format("RPS: SeasonId:~p SubjectId:~p", [FrpSeasonId, FrpSubjectId])), % Season and subject in RPS.
	dig:log(info, io_lib:format("From RPS: ~p", [length(FrpStudentList)-1])), % Header in rps list
	dig:log(info, io_lib:format("Missing found: ~p", [length(FrpStudentListMissing)])),
	dig:log(info, io_lib:format("Already exist: ~p", [length(OsmCandidateDocs)])),



	{ListOfFsToSave, _} = lists:foldl(fun([PRN, Name | _] = Row, {FsAcc, EpochTimeAcc}) ->
		Name1 = helper:replace_this_with_that(Name, "\"", ""),
		Fs = [
			itf:build(
				itf:textbox(?F(anpseatnumber)),
				import_anpseatnumber(
					PRN, dict:find(PRN, ProfileDocsDict), SeatNumberId
				)
			),
			itf:build(itf:textbox(?F(anpfullname)), Name1),
			itf:build(itf:textbox(?F(anpcentercode)), "0"),
			itf:build(itf:textbox(?F(anpstate)), import_anpstate(Row, BundleId)),
			itf:build(itf:textbox(?F(osm_bundle_fk)), import_bundleid(Row, BundleId)),
			itf:build(itf:textbox(?F(timestamp_inward)), ?I2S(EpochTimeAcc))
		],
		{FsAcc ++ [Fs], EpochTimeAcc +1}
	end, {[], EpochTime}, FrpStudentListMissing),
	{ok, Res} = anpcandidates:savebulk(ExamDb, ListOfFsToSave),


	%
	% update status
	%
	{OKs, Errors} = db_helper:bulksave_summary(Res),
	dig:log(success, io_lib:format("Saved. Oks: ~p, Errors: ~p", [OKs, Errors])).




%..............................................................................
%
% handle - ensure exam doc exists
%
%..............................................................................

handle_import_from_frp_examdoc_ensure_examdoc_exists(DateOfExam, OsmSeasonDoc, FrpSubjectDoc, FrpExamDoc) ->

	%
	% init
	%
	SeasonId = itf:idval(OsmSeasonDoc),
	SubjectId = itf:idval(FrpSubjectDoc),
	SubjectCode = itf:val(FrpSubjectDoc, subject_code),
	SubjectPattern = itf:val(FrpSubjectDoc, pattern),
	ExamName = itf:val(FrpExamDoc, exam_name),
	MarkType = itf:val2(FrpExamDoc, mark_type_fk),

	FacultyId = itf:val2(FrpExamDoc, faculty_code_fk),
	ProgramId = itf:val2(FrpExamDoc, program_code_fk),

	TestId = db:get_uuid(),

	%
	% find osm exam docs
	%
	FsFind = [
		fields:build(season_fk, SeasonId),
		fields:build(anptestcourseid, SubjectCode),
		fields:build(exam_pattern, SubjectPattern)
	],
	#db2_find_response {docs=OsmExamDocs0} = db2_find:get_by_fs(
		anptests:getdb(), FsFind, 0, ?INFINITY
	),

	OsmExamDocs = lists:filter(fun(EDoc) ->
		(?A2L(get_mark_type(EDoc)) == MarkType) or
		(itf:val2(EDoc, marktype) == MarkType) or
		((itf:val2(EDoc, marktype) == "end_exam_marks") and (MarkType == []))
	end, OsmExamDocs0),

	%
	% create or skip
	%
	case OsmExamDocs of
		[] ->
			S3Dir = ep_osm_exam_api:s3dir_new(
				OsmSeasonDoc, FrpSubjectDoc, TestId
			),
			FsToCreate = [
				fields:build('_id', TestId),
				fields:build(marktype, MarkType),
				fields:build(season_fk, SeasonId),

				fields:build(faculty_code_fk, FacultyId),
				fields:build(program_code_fk, ProgramId),
				fields:build(subject_code_fk, SubjectId),

				fields:build(aws_s3_dir, S3Dir),
				fields:build(anptestcourseid, SubjectCode),
				fields:build(testname, ExamName),
				fields:build(testdescription, ExamName),
				fields:build(teststatus, ?SCHEDULED),
				fields:build(testtotalmarks, "0"),
				fields:build(testduration, "0"),
				fields:build(startdate, DateOfExam),
				fields:build(enddate, DateOfExam),
				fields:build(pages_per_booklet, "0"),
				fields:build(exam_pattern, SubjectPattern)
			],
			{ok,  OsmExamDoc0} = ep_osm_exam_api:create(FsToCreate),
			dig:log(success, "Created test for subject " ++ SubjectCode),
			{ok,  OsmExamDoc0};
		[OsmExamDoc0] ->
			dig:log(info, "Found test for subject " ++ SubjectCode ++ " id:" ++ itf:idval(OsmExamDoc0)),
			{ok, OsmExamDoc0};
		_ ->
			dig:log(error, "Skip: Multiple exam docs found on OSM for subject code " ++ SubjectCode),
			skip
	end.



%..............................................................................
%
% handle - ensure season exists
%
%..............................................................................

handle_import_from_frp_examdoc_ensure_season_exists(FrpSeasonDoc) ->

	%
	% init
	%
	SeasonId = itf:idval(FrpSeasonDoc),
	SeasonCode = string:to_upper(helper:sanitisestr(itf:val(FrpSeasonDoc, name))),

	%
	% create if does not exists
	%
	case ep_core_exam_season_api:get(SeasonId) of
		{ok, Doc} ->
			dig:log(info, "Examseason exits, id:" ++ itf:idval(Doc)),
			{ok, Doc};
		_ ->
			FsToCreate = itf:d2f(FrpSeasonDoc, [
				itf:id(),
				?COREXS(name),
				?COREXS(description),
				?COREXS(state),
				?COREXS(year),
				?COREXS(startdate),
				?COREXS(type),
				?COREXS(enddate)
			]) ++ [
				itf:build(?COREXS(code), SeasonCode)
			],
			{ok, OsmSeasonDoc} = ep_core_exam_season_api:save(FsToCreate),
			dig:log(success, io_lib:format("Season created: ~s, ~s, ~s", [
				itf:val(OsmSeasonDoc, name),
				itf:val(OsmSeasonDoc, state),
				itf:val(OsmSeasonDoc, year)
			])),
			{ok, OsmSeasonDoc}
	end.


%..............................................................................
%
% handle - ensure subject exists
%
%..............................................................................

handle_import_from_frp_examdoc_ensure_subject_exists(FrpSubjectDoc) ->

	%
	% init
	%
	SubjectId = itf:idval(FrpSubjectDoc),


	%
	% create if does not exists
	%
	case ep_core_subject_api:get(SubjectId) of
		{ok, Doc} ->
			dig:log(info, "Subject doc exits, id:" ++ itf:idval(Doc)),
			{ok, Doc};
		_ ->
			FsToCreate = itf:d2f(FrpSubjectDoc, [
				itf:id(),
				?CORSUB(subject_code),
				?CORSUB(subject_name),
				?CORSUB(subject_description),
				?CORSUB(subject_short_name),
				?CORSUB(state),
				?CORSUB(pattern)
			]),
			{ok, OsmSubjectDoc} = ep_core_subject_api:save(FsToCreate),
			dig:log(success, io_lib:format("Subject created: ~s, ~s, ~s", [
				itf:val(OsmSubjectDoc, name),
				itf:val(OsmSubjectDoc, state),
				itf:val(OsmSubjectDoc, year)
			])),
			{ok, OsmSubjectDoc}
	end.


%..............................................................................
%
% handle - action import
%
%..............................................................................

handle_action_import() ->
	%
	% build form
	%
	Es = itl:get(?CREATE, fs(import), ite:get(import_from_frp, "Create"), table),


	%
	% build header
	%
	EsHeader = [
		#button {
			class="btn btn-sm btn-primary-outline pull-sm-right",
			text="Close",
			actions=[
				#event {
					type=click,
					actions=#update {target=panel_actions, elements=[]}
				}
			]
		},
		#p {
			class="font-weight-bold",
			text="Import"
		}
	],


	%
	% show form
	%
	Es1 = itl:section(EsHeader, Es),
	wf:update(panel_actions, Es1).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%
% sanitise prn
%
sanitise_prn("PRN:" ++ PRN) ->
	PRN;
sanitise_prn(PRN) ->
	PRN.



%
% rpc call
%
rpc_call(Node, Module, Function, Args) ->
	case configs:getatom(uniapps_deployment_type, standalone) of
		cluster ->
			erlang:apply(Module, Function, Args);
		_ ->
			rpc:call(Node, Module, Function, Args)
	end.



%
% sanitise frp list
%
sanitise_frp_list(FrpStudentList) ->
	lists:foldl(fun([PRN | Tail], Acc) ->
		case string:to_lower(PRN) of
			"enrol" ++ _ ->
				Acc;
			"\"enrol" ++ _ ->
				Acc;
			_ ->
				PRN1 = sanitise_prn(PRN),
				Acc ++ [
					[PRN1 | Tail]
				]
		end
	end, [], FrpStudentList).



%
% get prn from from list
%
get_prns_from_frp_list(List) ->
	lists:map(fun([PRN | _]) ->
		PRN
	end, List).



%
% get mark type id
%
get_mark_type(OsmExamDoc) ->
	case itf:val(OsmExamDoc, marktype) of
		[] ->
			end_exam_marks;
		MarkTypeStr ->
			?L2A(MarkTypeStr)
	end.



%
% get demo frp list for testing purposes
%
get_demo_frp_list(Size) ->
	UId = helper:uidintstr(),
	lists:map(fun(Index) ->
		[
			itx:format("~s_~p", [UId, Index]), itx:format("Name ~p", [Index]), ""
		]
	end, lists:seq(1, Size)) ++ [
		[
			itx:format("AB_~s", [UId]), itx:format("Name AB~s", [UId]), "ab"
		]
	].



%
% get profiles docs dict from missing prn
%
get_profile_docs_dict_from_frp_list(_FrpStudentListMissing, prn) ->
	dict:new();
get_profile_docs_dict_from_frp_list(FrpStudentListMissing, _) ->
	MissingPRNs = lists:map(fun([PRN | _]) ->
		sanitise_prn(PRN)
	end, FrpStudentListMissing),
	ProfileDocs = itxprofiles:getdocs_by_usernames(MissingPRNs),
	helper:get_dict_from_docs(ProfileDocs, prn).


%
% save frp list
%
save_frp_list_to_file(FrpStudentList) ->

	%
	% init
	%
	FrpStudentList1 = lists:map(fun(Line) ->
		string:join(Line, ",")
	end, FrpStudentList),
	FrpStudentList2 = string:join(FrpStudentList1, "\n"),

	%
	% write to file
	%
	Filename = itx:format("rps_list_~s.csv", [helper:uidintstr()]),
	Filepath = itx:format("/tmp/~s", [Filename]),
	ok = file:write_file(Filepath, helper:l2b(FrpStudentList2)),


	%
	% zip and upload to minijob
	%
	case erlang:get(minijobcontext) of
		undefined ->
			skip;
		MinijobDoc ->
			FilenameZip = itx:format("~s.zip", [Filename]),
			FilepathZip = itx:format("/tmp/~s", [FilenameZip]),
			helper:cmd("cd /tmp; zip ~s ~s", [FilenameZip, Filename]),
			{ok, _} = minijob_api:upload_file(MinijobDoc, FilenameZip, FilepathZip)
	end.



%------------------------------------------------------------------------------
% import functions
%------------------------------------------------------------------------------


%
% import anp seat number
%
import_anpseatnumber(_PRN, {ok, Doc}, SeatNumberId) when 
	SeatNumberId /= prn ->
	itf:val(Doc, SeatNumberId);
import_anpseatnumber(PRN, _, _) ->
	PRN.


%
% import anpstate
%
import_anpstate(Row, BundleId) ->
	LastColumn = lists:last(Row),
	LastColumn1 = helper:replace_this_with_that(LastColumn, "\"", ""),
	case  {string:to_lower(LastColumn1), BundleId} of
		{"ab", _} ->
			"anpstate_absent";
		{_, BundleId} when is_list(BundleId) ->
			"anpstate_not_uploaded";
		_ ->
			"anpstate_expected"
	end.


%
% import bundleid
%
import_bundleid(Row, BundleId) ->
	LastColumn = lists:last(Row),
	LastColumn1 = helper:replace_this_with_that(LastColumn, "\"", ""),
	case  string:to_lower(LastColumn1) of
		"ab" ->
			"";
		_ ->
			BundleId
	end.



%------------------------------------------------------------------------------
% create 
%------------------------------------------------------------------------------

create_bundle(?YES, SeasonId, ExamId) ->

	%
	% fields to save
	%
	FsToSave = [
		itf:build(?COREXS(season_fk), SeasonId),
		itf:build(?OSMBDL(osm_exam_fk), ExamId),
		itf:build(?OSMBDL(inward_date), helper:date_today_str()),
		itf:build(?OSMBDL(scanned_date), ""),
		itf:build(?OSMBDL(uploaded_date), ""),
		itf:build(?OSMBDL(qc_date), ""),
		itf:build(?OSMBDL(inwardstate), "completed"),
		itf:build(?OSMBDL(scanningstate), ""),
		itf:build(?OSMBDL(uploadstate), ""),
		itf:build(?OSMBDL(qcstate), ""),
		itf:build(?OSMBDL(packet_number), ""),
		itf:build(?OSMBDL(packet_count), ""),
		itf:build(?OSMBDL(rack_location), ""),
		itf:build(?OSMBDL(receivedby), ""),
		itf:build(?OSMBDL(receivedon), ""),
		itf:build(?OSMBDL(createdby), itxauth:user()),
		itf:build(?OSMBDL(createdon), helper:epochtimestr())
	],


	%
	% save
	%
	{ok,  {ok, BundleDoc}} = ep_osm_bundle_api:create(FsToSave),
	itf:idval(BundleDoc);
create_bundle(_, _, _) ->
	undefined.



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
