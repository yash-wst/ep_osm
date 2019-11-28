
-module(dig_ep_osm_exam_evaluator_stats).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Exam Evaluation Statistics").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
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
			itf:hidden(osm_exam_fk)
		],
		size=50
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exam Evaluation Statistics").



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
	dig_ep_osm_exam_evaluation_stats:fetch(D, From, Size, Fs);


%..............................................................................
%
% []
%
%..............................................................................
fetch(D, _From, _Size, []) ->
	Fs = [
		fields:build(teststatus, ?ACTIVE)
	],
	fetch(D, 0, ?INFINITY, Fs);



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
	SeasonDocsDict = ep_core_exam_season_api:get_dict(Docs),
	FacultyDocsDict = ep_core_faculty_api:get_dict(Docs),
	ProgramDocsDict = ep_core_program_api:get_dict(Docs),
	SubjectDocsDict = ep_core_subject_api:get_dict(Docs),


	%
	% results
	%
	Results = lists:map(fun(Doc) ->


		%
		% init
		%
		TFs = helper_api:doc2fields({ok, Doc}),
		SeasonDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, season_fk), SeasonDocsDict),
		FacultyDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, faculty_code_fk), FacultyDocsDict),
		ProgramDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, program_code_fk), ProgramDocsDict),
		SubjectDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, subject_code_fk), SubjectDocsDict),



		%
		% get stats
		%
		Stats = ep_osm_exam_api:get_evaluation_stats0(itf:idval(Doc)),
		StatsDict = dict:from_list(Stats),


		%
		% layout test
		%
		[
			#dcell {val=itl:blockquote(SeasonDoc, [?COREXS(name), ?COREXS(state)])},
			#dcell {val=itl:blockquote(FacultyDoc, [?CORFAC(faculty_code), ?CORFAC(faculty_name)])},
			#dcell {val=itl:blockquote(ProgramDoc, [?CORPGM(program_code), ?CORPGM(program_name)])},
			#dcell {val=itl:blockquote(SubjectDoc, [?CORSUB(subject_code), ?CORSUB(subject_name)])},
			#dcell {
				val=itl:blockquote([
					itf:val(Doc, anptestcourseid)
				]),
				postback={filter, itf:build(?OSMEXM(osm_exam_fk), itf:idval(Doc))}
			}

		] ++ lists:map(fun(RoleId) ->
				ProfileIds = anpcandidates:get_evaluators_for_test(TFs, RoleId),
				Val = layout_cell(RoleId, StatsDict, ProfileIds),
				#dcell {
					val_export=length(helper:unique(ProfileIds)),
					val=Val
				}
		end, [
			anpevaluator,
			anpmoderator,
			anprevaluator,
			anpmoderator_reval
		])


	end, Docs),


	%
	% sort results
	%
	ResultsSorted = lists:sort(fun(A, B) ->
		#dcell {val=YetToStartA} = lists:nth(7, A),
		#dcell {val=YetToStartB} = lists:nth(7, B),
		YetToStartA > YetToStartB
	end, Results),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Season"},
		#dcell {type=header, val="Faculty"},
		#dcell {type=header, val="Program"},
		#dcell {type=header, val="Subject"},
		#dcell {type=header, val="Test Id"},
		#dcell {type=header, val=layout_cell_header(anpevaluator)},
		#dcell {type=header, val=layout_cell_header(anpmoderator)},
		#dcell {type=header, val=layout_cell_header(anprevaluator)},
		#dcell {type=header, val=layout_cell_header(anpmoderator_reval)},
		#dcell {type=header, val="Total"}
	],



	%
	% return
	%
	{
		D#dig {
			total=?INFINITY
		},
		[Header] ++ dig:append_total_cells(ResultsSorted)
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
% layout cell header
%
%..............................................................................

layout_cell_header(RoleId) ->
	#table {
		rows=[
			#tablerow {cells=[
				#tablecell {
					colspan=?CASE_IF_THEN_ELSE(RoleId, anpevaluator, 6, 2),
					text=?LN(RoleId)
				}
			]},
			#tablerow {cells=lists:map(fun(State) ->
				#tablecell {
					text=?LN(?L2A(?A2L(State) ++ "_min"))
				}
			end, states(RoleId))}
		]
	}.



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

	#table {
		rows=[
			#tablerow {cells=[
				#tablecell {
					class="mycenter " ++ get_class_evaluator(length(ProfileIds), TotalPapers),
					colspan=?CASE_IF_THEN_ELSE(RoleId, anpevaluator, 6, 2),
					text=length(helper:unique(ProfileIds))
				}
			]},
			#tablerow {cells=lists:map(fun(State) ->
				Val = dict:find([?A2L(State)], StatsDict),
				#tablecell {
					class=get_class(Val),
					text=get_val(Val)
				}
			end, states(RoleId))}
		]
	}.


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


%
% get class evaluator
%
get_class_evaluator(TotalEvaluators, _) when TotalEvaluators > 0 ->
	"bg-success";
get_class_evaluator(0, TotalPapers) when TotalPapers > 0 ->
	"bg-danger";
get_class_evaluator(_, _) ->
	[].

%
% get class
%
get_class({ok, Number}) when Number > 0 ->
	"bg-info";
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
