
-module(dig_ep_osm_exam_upload_to_result_processing_system).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


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

f(frp_season_fk = I) ->
	F = ?COREXS(season_fk),
	F#field {
		id=I,
		label="Season - Result Processing System",
		options=F#field.options#search{node=itxnode:frp()}
	}.


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
	anptestcourseid
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
		end, itf:d2f(Doc, FsDisplay))
	end, Docs),


	Header = lists:map(fun(F) ->
		#dcell {type=header, val=F#field.label}
	end, FsDisplay),


	%
	% return
	%
	{
		D#dig {
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
	FEvaluatorType = fields:get(osm_profiletype),
	FsUpload = [
		f(frp_season_fk),
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
	Context = wf_context:context(),
	OsmSeasonFk = wf:q(season_fk),
	FrpSeasonFk = wf:q(frp_season_fk),
	OsmEvaluatorType = wf:q(osm_profiletype),


	%
	% function
	%
	Fun = fun([]) ->
		wf_context:context(Context),
		handle_upload(OsmSeasonFk, FrpSeasonFk, OsmEvaluatorType)
	end,


	%
	% add to queue
	%
	taskqueue:create(Fun, []),
	helper_ui:flash(warning, "Added to queue.", 5).



handle_upload(OsmSeasonFk, FrpSeasonFk, OsmEvaluatorType) ->


	%
	% ensure seasons have same name
	%
	{ok, OsmSeasonDoc} = ep_core_exam_season_api:get(OsmSeasonFk),
	{ok, FrpSeasonDoc} = rpc:call(
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
			{ok, FrpSeasonDocRegular} = rpc:call(
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

	Dig = helper:state(dig),
	FsFilters = dig:get_nonempty_fs(Dig#dig.filters),
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
		handle_upload(OsmSeasonFk, FrpSeasonDoc, OsmEvaluatorType, Doc)
	end, Docs).





handle_upload(_OsmSeasonFk, FrpSeasonDoc, OsmEvaluatorType, Doc) ->

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

	SubjectDocs = rpc:call(
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
			handle_upload_marks(FrpSeasonDoc, OsmEvaluatorType, Doc, MatchingSubjectDoc);
		_ ->
			dig:log(error, io_lib:format("Error! Subject (~s, ~s) has multiple documents on result processing system", [
				SubjectCode, Pattern
			]))
	end.





handle_upload_marks(FrpSeasonDoc, OsmEvaluatorType, OsmExamDoc, MatchingSubjectDoc) ->

	%
	% init
	%
	FrpSeasonFk = itf:idval(FrpSeasonDoc),
	FrpSeasonType = itf:val(FrpSeasonDoc, type),
	ExamId = itf:idval(OsmExamDoc),
	SubjectId = itf:idval(MatchingSubjectDoc),
	SubjectCode = itf:val(OsmExamDoc, anptestcourseid),
	Pattern = itf:val(OsmExamDoc, exam_pattern),
	dig:log(warning, io_lib:format("Uploading ... (~s, ~s)", [SubjectCode, Pattern])),


	%
	% decide marktype to upload based on exam season type
	%
	MarkTypeId = case FrpSeasonType of
		"revaluation" ->
			revaluation_marks;
		_ ->
			end_exam_marks
	end,

	%
	% get results data
	%
	{CsvDataSize, CsvData} = ep_osm_exam_api:csv_frp(ExamId, OsmEvaluatorType),


	%
	% post it on result processing system
	%
	RpcRes = rpc:call(
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
				fields:build(result_upload_status, io_lib:format("uploaded ~s, ~p", [
					OsmEvaluatorType, MarkTypeId
				]))
			],
			{ok, _} = ep_osm_exam_api:save(
				FsToSave, ep_osm_exam:fs(all), ExamId
			),
			dig:log(success, io_lib:format("(~s, ~s) added to queue", [SubjectCode, Pattern]))
	end.


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
