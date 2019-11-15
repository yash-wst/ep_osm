
-module(dig_ep_osm_exam_upload_to_result_processing_system).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

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
	handle_upload(wf:q(season_fk), wf:q(frp_season_fk));

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
	FsUpload = [
		f(frp_season_fk)
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


handle_upload(OsmSeasonFk, FrpSeasonFk) ->

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
		handle_upload(OsmSeasonFk, FrpSeasonFk, Doc)
	end, Docs).





handle_upload(_OsmSeasonFk, FrpSeasonFk, Doc) ->

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
			handle_upload_marks(FrpSeasonFk, Doc, MatchingSubjectDoc);
		_ ->
			dig:log(error, io_lib:format("Error! Subject (~s, ~s) has multiple documents on result processing system", [
				SubjectCode, Pattern
			]))
	end.





handle_upload_marks(FrpSeasonFk, OsmExamDoc, MatchingSubjectDoc) ->

	%
	% init
	%
	ExamId = itf:idval(OsmExamDoc),
	MarkTypeId = end_exam_marks,
	SubjectId = itf:idval(MatchingSubjectDoc),
	SubjectCode = itf:val(OsmExamDoc, anptestcourseid),
	Pattern = itf:val(OsmExamDoc, exam_pattern),
	dig:log(warning, io_lib:format("Uploading ... (~s, ~s)", [SubjectCode, Pattern])),


	%
	% get results data
	%
	{CsvDataSize, CsvData} = ep_osm_exam_api:csv_frp(ExamId),


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
				fields:build(result_upload_status, "uploaded")
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
