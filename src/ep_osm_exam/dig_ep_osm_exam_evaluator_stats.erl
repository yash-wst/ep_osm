
-module(dig_ep_osm_exam_evaluator_stats).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
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
			itf:build(itf:hidden(osm_exam_fk), itxcontext:q(id))
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
	#field {id=osm_exam_fk}
	] = Fs) ->
	{D1, Results} = dig_ep_osm_exam_evaluation_stats:fetch(D, From, Size, Fs),
	{D1#dig {actions=[]}, Results};


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


	%
	% zip and mail dir
	%
	helper:zip_mail_clean_dir([Email], Dir, "OSM: Evaluator statistics export"),
	dig:log(success, "Task completed.").



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
			module=dig_ep_osm_exam_evaluation_stats,
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




%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
