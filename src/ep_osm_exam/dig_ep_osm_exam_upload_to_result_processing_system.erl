
-module(dig_ep_osm_exam_upload_to_result_processing_system).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-import(dig_mm_ep_osm_exam_from_frp, [
	rpc_call/4
]).


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("Result Upload To Result Processing System").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------


f(osm_profiletype) ->
	itf:dropdown(?F(osm_profiletype, "Upload Marks Of"), itf:options([
		?F(profiletype_anpevaluator, "Evaluator"),
		?F(profiletype_anpmoderator, "Moderator"),
		?F(profiletype_anprevaluator, "Revaluator"),
		?F(profiletype_anpmoderator_reval, "Moderator-Reval"),
		?F(dtp_marks_manual, "DTP - Manual"),
		?F(dtp_marks_omr, "DTP - OMR")
	]));


f(frp_season_fk = I) ->
	F = ?COREXS(season_fk),
	F#field {
		id=I,
		label="Season - Result Processing System",
		options=F#field.options#search{node=itxnode:frp()}
	};

f(frp_mark_type = I) ->
	itf:dropdown(?F(I, "Mark type"), options(frp_mark_type)).



options(frp_mark_type) ->
	Docs = rpc_call(
		itxnode:frp(),
		up_core_mark_type_api,
		get_marktype_docs,
		[]
	),
	?ASSERT(Docs =/= undefined, "Mark types not created!"),
	L = [?F(itf:val2(Doc, mtype_id), itf:val2(Doc, lable)) || Doc <- Docs],
	itf:options(L).



%
% fs
%

fs(search) ->
	fields:getfields(fids(search));


fs(minijob) -> [
	f(frp_season_fk),
	f(osm_profiletype),
	f(frp_mark_type)
].


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, _) -> false.



fids(display) -> [
	season_fk,
	faculty_code_fk,
	program_code_fk,
	subject_code_fk,
	anptestcourseid,
	teststatus,
	result_upload_status
];

fids(search) -> [
	season_fk,
	faculty_code_fk,
	program_code_fk,
	subject_code_fk,
	anptestcourseid,
	startdate
].

%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=fields:getfields(fids(search))
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("Result Upload To Result Processing System").



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
fetch(D, _From, _Size, []) ->
	{
		D,
		[{custom, #panel {
			style="margin-top: 10px;",
			class="mycenter",
			body=layout:frame("Please select filters to search & upload.")
		}}]
	};


fetch(D, _From, _Size, Fs) ->

	%
	% init
	%
	FsFind = Fs ++ [
		fields:build(teststatus, "completed"),
		db2es_find:get_field_cond("$in", result_upload_status, ["", "pending"])
	],


	%
	% get all docs matching search criteria
	%
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		anptests:getdb(), FsFind, 0, ?INFINITY
	),



	%
	% layout
	%
	FsDisplay = fields:getfields(fids(display)),
	Results = lists:map(fun(Doc) ->
		lists:map(fun(F) ->
			#dcell {val=itl:render(F)}
		end, itf:d2f(Doc, FsDisplay)) ++ [
			#dcell {
				val=helper_ui:layout_slinks(anptest, itf:d2f(Doc, anptest:fs(index)))
			}
		]
	end, Docs),


	Header = lists:map(fun(F) ->
		#dcell {type=header, val=F#field.label}
	end, FsDisplay) ++ [
		#dcell {type=header, val="View"}
	],


	%
	% return
	%
	{
		D#dig {
			description="Completed exams; result upload pending.",
			total=length(Docs),
			actions=[
				{action_upload_to_frp, "Upload Results", "Upload Results"}
			]
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

event(upload) ->
	handle_upload();

event(action_upload_to_frp) ->
	handle_action_upload_to_frp();

event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - action upload to frp
%
%..............................................................................

handle_action_upload_to_frp() ->
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
			text="Upload to result processing system"
		}
	],


	%
	% build form
	%
	FEvaluatorType = f(osm_profiletype),
	FsUpload = [
		f(frp_season_fk),
		f(frp_mark_type),
		FEvaluatorType#field {label="Upload Marks Of"}
	],
	Es = [
		itl:get(?EDIT, FsUpload, ite:get(upload, "Upload"), table)
	],


	%
	% show form
	%
	Es1 = itl:section(EsHeader, Es),
	wf:update(panel_actions, Es1).




%..............................................................................
%
% handle upload
%
%..............................................................................


handle_upload() ->


	%
	% init
	%
	OsmSeasonFk = wf:q(season_fk),
	FrpSeasonFk = wf:q(frp_season_fk),
	FrpMarkType = wf:q(frp_mark_type),
	OsmEvaluatorType = wf:q(osm_profiletype),


	case configs:getbool(process_via_minijob, false) of
		false ->
			handle_upload_via_taskqueue(
				OsmSeasonFk, FrpSeasonFk, FrpMarkType, OsmEvaluatorType
			);
		true ->
			handle_upload_via_minijob()
	end.


%
% handle upload - taskqueue
%
handle_upload_via_taskqueue(OsmSeasonFk, FrpSeasonFk, FrpMarkType, OsmEvaluatorType) ->

	%
	% init
	%
	Context = wf_context:context(),
	Dig = helper:state(dig),
	FsFilters = dig:get_nonempty_fs(Dig#dig.filters),

	%
	% function
	%
	Fun = fun([]) ->
		wf_context:context(Context),
		handle_upload_actual(OsmSeasonFk, FrpSeasonFk, OsmEvaluatorType, FrpMarkType, FsFilters)
	end,


	%
	% add to queue
	%
	taskqueue:create(Fun, []),
	helper_ui:flash(warning, "Added to queue.", 5).




%
% handle upload - minijob
%
handle_upload_via_minijob() ->
	{ok, Doc} = minijob_upload_to_rps:create_and_run(
		itf:uivalue(fs(search) ++ fs(minijob))
	),
	minijob_status:show_status(Doc).




%
% handle uplaod 
%
handle_upload_actual(OsmSeasonFk, FrpSeasonFk, OsmEvaluatorType, FrpMarkType, FsFilters) ->


	%
	% ensure seasons have same name
	%
	{ok, OsmSeasonDoc} = ep_core_exam_season_api:get(OsmSeasonFk),
	{ok, FrpSeasonDoc} = rpc_call(
		itxnode:frp(),
		ep_core_exam_season_api,
		get,
		[FrpSeasonFk]
	),
	FrpSeasonType = itf:val(FrpSeasonDoc, type),


	%
	% get regular season doc
	%
	FrpSeasonDoc1 = case FrpSeasonType of
		"revaluation" ->
			RegularSeasonId = itf:val(FrpSeasonDoc, revaluation_for_exam_season),
			{ok, FrpSeasonDocRegular} = rpc_call(
				itxnode:frp(),
				ep_core_exam_season_api,
				get,
				[RegularSeasonId]
			),
			FrpSeasonDocRegular;
		_ ->
			FrpSeasonDoc
	end,


	%
	% season should match
	%
	?ASSERT(
		(
			(itf:idval(OsmSeasonDoc) == itf:idval(FrpSeasonDoc1)) and
			(itf:val(OsmSeasonDoc, name) == itf:val(FrpSeasonDoc1, name)) and
			(itf:val(OsmSeasonDoc, type) == itf:val(FrpSeasonDoc1, type)) and
			(itf:val(OsmSeasonDoc, state) == itf:val(FrpSeasonDoc1, state))
		),
		"Error! Season mismatch. id, Name, type and state should match between OSM & FRP seasons."
	),



	%
	% get all completed but not uploaded exams
	%
	FsFind = FsFilters ++ [
		fields:build(teststatus, "completed"),
		db2es_find:get_field_cond("$in", result_upload_status, ["", "pending"])
	],

	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		anptests:getdb(), FsFind, 0, ?INFINITY
	),


	%
	% handle upload of each exam doc
	%
	lists:foreach(fun(Doc) ->
		handle_upload(OsmSeasonFk, FrpSeasonDoc, OsmEvaluatorType, Doc, FrpMarkType)
	end, Docs).





handle_upload(_OsmSeasonFk, FrpSeasonDoc, OsmEvaluatorType, Doc, FrpMarkType) ->

	%
	% init
	%
	SubjectCode = itf:val(Doc, anptestcourseid),
	SubjectCodes = [SubjectCode],
	Pattern = itf:val(Doc, exam_pattern),
	dig:log(info, io_lib:format("Processing ... (~s, ~s)", [SubjectCode, Pattern])),


	%
	% find matching subject code and pattern on result processing system
	%

	SubjectDocs = rpc_call(
		itxnode:frp(),
		ep_core_subject_api,
		getdocs_by_subject_codes,
		[SubjectCodes]
	),

	MatchingSubjectDocs = lists:filter(fun(SubjectDoc) ->
		(itf:val(SubjectDoc, subject_code) == SubjectCode) and
		(itf:val(SubjectDoc, pattern) == Pattern)
	end, SubjectDocs),



	%
	% process found subject docs
	%

	case MatchingSubjectDocs of
		[] ->
			dig:log(error, io_lib:format("Error! Subject (~s, ~s) not found on result processing system", [
				SubjectCode, Pattern
			]));
		[MatchingSubjectDoc] ->
			handle_upload_marks(FrpSeasonDoc, OsmEvaluatorType, Doc, MatchingSubjectDoc, FrpMarkType);
		_ ->
			dig:log(error, io_lib:format("Error! Subject (~s, ~s) has multiple documents on result processing system", [
				SubjectCode, Pattern
			]))
	end.





handle_upload_marks(FrpSeasonDoc, OsmEvaluatorType, OsmExamDoc, MatchingSubjectDoc) ->
	FrpSeasonType = itf:val(FrpSeasonDoc, type),
	%
	% decide marktype to upload based on exam season type
	%
	MarkTypeId = case FrpSeasonType of
		"revaluation" ->
			revaluation_marks;
		_ ->
			end_exam_marks
	end,

	handle_upload_marks(FrpSeasonDoc, OsmEvaluatorType, OsmExamDoc, MatchingSubjectDoc, MarkTypeId).


handle_upload_marks(FrpSeasonDoc, OsmEvaluatorType, OsmExamDoc, MatchingSubjectDoc, MarkTypeId) when MarkTypeId==undefined; MarkTypeId==[] ->
	handle_upload_marks(FrpSeasonDoc, OsmEvaluatorType, OsmExamDoc, MatchingSubjectDoc);


handle_upload_marks(FrpSeasonDoc, OsmEvaluatorType, OsmExamDoc, MatchingSubjectDoc, MarkTypeId0) ->

	%
	% init
	%
	FrpSeasonFk = itf:idval(FrpSeasonDoc),
	ExamId = itf:idval(OsmExamDoc),
	SubjectId = itf:idval(MatchingSubjectDoc),
	SubjectCode = itf:val(OsmExamDoc, anptestcourseid),
	Pattern = itf:val(OsmExamDoc, exam_pattern),
	dig:log(warning, io_lib:format("Uploading ... (~s, ~s)", [SubjectCode, Pattern])),

	MarkTypeId = case is_list(MarkTypeId0) of
		true -> ?L2A(MarkTypeId0);
		_ -> MarkTypeId0
	end,

	%
	% get results data
	%
	{CsvDataSize, CsvData} = csv_frp(ExamId, OsmEvaluatorType),


	%
	% post it on result processing system
	%
	RpcRes = rpc_call(
		itxnode:frp(),
		up_core_marks_upload_queue_api,
		create_rpc,
		[FrpSeasonFk, SubjectId, MarkTypeId, ?L2B(CsvData), CsvDataSize]
	),
	case RpcRes of
		{badrpc, Reason} ->
			dig:log(error, io_lib:format("Failed! ~p", [Reason]));
		_ ->

			%
			% update state
			%
			FsToSave = [
				fields:build(
					result_upload_status,
					get_result_upload_status(OsmEvaluatorType, MarkTypeId)
				)
			],
			{ok, _} = ep_osm_exam_api:save(
				FsToSave, ep_osm_exam:fs(all), ExamId
			),
			dig:log(success, io_lib:format("(~s, ~s) added to queue", [SubjectCode, Pattern]))
	end.


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% csv frp
%
csv_frp(ExamId, OsmEvaluatorType) ->
	csv_frp(ExamId, OsmEvaluatorType, itxconfigs:get2(ep_osm_result_csv_frp_ids, undefined)).

csv_frp(ExamId, OsmEvaluatorType, undefined) ->
	ep_osm_exam_api:csv_frp(ExamId, OsmEvaluatorType);
csv_frp(ExamId, OsmEvaluatorType, Ids) ->

	%
	% init
	%
	CompletedState = case OsmEvaluatorType of
		"profiletype_" ++ Role ->
			ep_osm_helper:completed_state_of(Role);
		_ ->
			"anpstate_completed"
	end,


	TotalMarksId = case OsmEvaluatorType of
		"profiletype_anp" ++ Role1 ->
			Role1 ++ "_total";
		_ ->
			OsmEvaluatorType
	end,
	Ids1 = helper:list_search_replace_elem("total", TotalMarksId, Ids),


	%
	% create dig for export
	%
	D = #dig {
		module=dig_ep_osm_exam_results,
		size=200,
		filters=[
			itf:build(itf:hidden(osm_exam_fk), ExamId),
			fields:build(anpstate, CompletedState)
		],
		config=[
			{ep_osm_result_exportids, string:join(Ids1, ",")}
		]
	},


	%
	% create file
	%
	{ok, Doc} = ep_osm_exam_api:get(ExamId),
	{_Name, FilePath} = dig_ep_osm_exam_results:handle_export_results_bulk_create_file(Doc, D),
	ListOfList = helper_csv_parser:parse(FilePath),


	%
	% filter out entries where prn is empty
	%
	ListOfList1 = lists:filter(fun([PRN |_]) ->
		PRN /= []
	end, ListOfList),


	%
	% return csvdata
	%
	Lines = lists:map(fun(List) ->
		string:join(List, ",")
	end, ListOfList1),
	{length(Lines), string:join(Lines, "\n")}.




%
% get result upload status
%
get_result_upload_status("profiletype_anpmoderator", _) ->
	"uploaded_moderation";
get_result_upload_status("profiletype_anprevaluator", _) ->
	"uploaded_revaluation";
get_result_upload_status("profiletype_anpmoderator_reval", _) ->
	"uploaded_moderation_reval";
get_result_upload_status("dtp_marks_manual", _) ->
	"uploaded_dtp_marks_manual";
get_result_upload_status("dtp_marks_omr", _) ->
	"uploaded_dtp_marks_omr";
get_result_upload_status(_OsmEvaluatorType, _MarkTypeId) ->
	"uploaded".



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
