
-module(dig_mm_ep_osm_exam_from_frp).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

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
	itf:date(?F(date_of_test, "Date of Test"))
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
		module=?MODULE,
		filters=fs(form),
		size=25
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

	%
	% init
	%
	DateOfExam = wf:q(date_of_test),
	FDate = itf:build(itf:date(?F(date)), DateOfExam),

	%
	% get exam from frp of matching date
	%
	{_, FrpExamDocs} = rpc:call(
		itxnode:frp(),
		up_core_exam_api,
		get_by_field,
		[FDate]
	),
	dig:log(info, io_lib:format("Found ~p exam docs on FRP system", [length(FrpExamDocs)])),


	%
	% remove duplicates of exams from frp
	%
	FrpExamDocsDict = helper:get_dict_from_docs(FrpExamDocs, subject_code_fk),
	FrpExamDocsUnique = dict:to_list(FrpExamDocsDict),


	%
	% for each doc
	%

	lists:foreach(fun({_, FrpExamDoc}) ->
			handle_import_from_frp_examdoc(DateOfExam, FrpExamDoc)
	end, lists:sublist(FrpExamDocsUnique, 4)),


	%
	% finish
	%
	dig:log(info, "Task completed").




%..............................................................................
%
% handle - import from frp exam doc
%
%..............................................................................

handle_import_from_frp_examdoc(DateOfExam, FrpExamDoc) ->

	%
	% init
	%
	SeasonId = itf:val(FrpExamDoc, season_fk),
	SubjectId = itf:val(FrpExamDoc, subject_code_fk),


	%
	% get season and subject docs from frp
	%
	{ok, FrpSeasonDoc} = rpc:call(
		itxnode:frp(),
		ep_core_exam_season_api,
		get,
		[SeasonId]
	),
	{ok, FrpSubjectDoc} = rpc:call(
		itxnode:frp(),
		ep_core_subject_api,
		get,
		[SubjectId]
	),


	dig:log(warning, "PROCESSING ... " ++ itf:val(FrpSubjectDoc, subject_code)),


	%
	% check if season doc exists else, create it
	%
	{ok, OsmSeasonDoc} = handle_import_from_frp_examdoc_ensure_season_exists(
		FrpSeasonDoc
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
		FrpExamDoc, ResOsmExamDoc
	).



%..............................................................................
%
% handle - upload student list
%
%..............................................................................

handle_import_from_frp_examdoc_upload_student_list(FrpExamDoc, {ok, OsmExamDoc}) ->


	%
	% init
	%
	dig:log(info, "Updating student list ... "),
	OsmExamId = itf:idval(OsmExamDoc),
	ExamDb = anpcandidates:db(OsmExamId),
	FrpSeasonId = itf:val(FrpExamDoc, season_fk),
	FrpSubjectId = itf:val(FrpExamDoc, subject_code_fk),


	%
	% get student list from osm exam
	%
	Db2FindRec = db2_find:getrecord_by_fs(
		ExamDb, [], 0, ?INFINITY
	),
	#db2_find_response {docs=OsmCandidateDocs} = db2_find:find(Db2FindRec#db2_find {
		fields=[
			itf:textbox(?F(anpseatnumber))
		]
	}),
	OsmCandidateDocsDict = helper:get_dict_from_docs(OsmCandidateDocs, anpseatnumber),



	%
	% get student list from frp
	%
	FrpStudentList = rpc:call(
		itxnode:frp(),
		dig_result_upload_handlers,
		handle_download_prns,
		[FrpSeasonId, FrpSubjectId, end_exam_marks]
	),


	%
	% find student list that does not exist on osm
	%
	FrpStudentListMissing = lists:filter(fun([PRN | _]) ->
		case string:to_lower(PRN) of
			"enrol" ++ _ ->
				false;
			"\"enrol" ++ _ ->
				false;
			_ ->
				dict:find(PRN, OsmCandidateDocsDict) == error
		end
	end, FrpStudentList),
	dig:log(info, io_lib:format("Missing found: ~p", [length(FrpStudentListMissing)])),


	%
	% create student list
	%
	ListOfFsToSave = lists:map(fun([PRN, Name | _]) ->
		Name1 = helper:replace_this_with_that(Name, "\"", ""),
		PRN1 = string:tokens(PRN, "PRN:"),
		[
			itf:build(itf:textbox(?F(anpseatnumber)), PRN1),
			itf:build(itf:textbox(?F(anpfullname)), Name1),
			itf:build(itf:textbox(?F(anpcentercode)), "0"),
			itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded")
		]
	end, FrpStudentListMissing),
	{ok, Res} = anpcandidates:savebulk(ExamDb, ListOfFsToSave),


	%
	% update status
	%
	{OKs, Errors} = db_helper:bulksave_summary(Res),
	dig:log(success, io_lib:format("Saved. Oks: ~p, Errors: ~p", [OKs, Errors]));



handle_import_from_frp_examdoc_upload_student_list(_, _) ->
	skip.


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
	SubjectCode = itf:val(FrpSubjectDoc, subject_code),
	ExamName = itf:val(FrpExamDoc, exam_name),


	%
	% find osm exam docs
	%
	FsFind = [
		fields:build(season_fk, SeasonId),
		fields:build(anptestcourseid, SubjectCode)
	],
	#db2_find_response {docs=OsmExamDocs} = db2_find:get_by_fs(
		anptests:getdb(), FsFind, 0, ?INFINITY
	),


	%
	% create or skip
	%
	case OsmExamDocs of
		[] ->
			S3Dir = ?FLATTEN(io_lib:format("~s/~s", [
				itf:val(OsmSeasonDoc, code), SubjectCode
			])),
			FsToCreate = [
				fields:build(season_fk, SeasonId),
				fields:build(aws_s3_dir, S3Dir),
				fields:build(anptestcourseid, SubjectCode),
				fields:build(testname, ExamName),
				fields:build(testdescription, ExamName),
				fields:build(teststatus, ?SCHEDULED),
				fields:build(testtotalmarks, "0"),
				fields:build(testduration, "0"),
				fields:build(startdate, DateOfExam),
				fields:build(enddate, DateOfExam),
				fields:build(pages_per_booklet, "0")
			],
			{ok,  OsmExamDoc0} = ep_osm_exam_api:create(FsToCreate),
			dig:log(success, "Created test for subject " ++ SubjectCode),
			{ok,  OsmExamDoc0};
		[OsmExamDoc0] ->
			dig:log(info, "Found test for subject " ++ SubjectCode),
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



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------