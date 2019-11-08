
-module(dig_ep_osm_exam_evaluation_stats).
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
% []
%
%..............................................................................
fetch(D, From, Size, [
	]) ->

	%
	% get active tests
	%
	Fs = [
		fields:build(teststatus, ?ACTIVE)
	],
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
		SeasonDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, season_fk), SeasonDocsDict),
		FacultyDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, faculty_code_fk), FacultyDocsDict),
		ProgramDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, program_code_fk), ProgramDocsDict),
		SubjectDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, subject_code_fk), SubjectDocsDict),


		%
		% get stats for test
		%
		Stats = ep_osm_exam_api:get_evaluation_stats(itf:idval(Doc)),
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

		] ++ lists:map(fun(State) ->
				Val = case dict:find([State], StatsDict) of
				{ok, Val0} ->
					Val0;
				_ ->
					0
				end,
				#dcell {
					bgcolor=get_class(State, Val),
					val=Val
				}
		end, states())


	end, Docs),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Season"},
		#dcell {type=header, val="Faculty"},
		#dcell {type=header, val="Program"},
		#dcell {type=header, val="Subject"},
		#dcell {type=header, val="Test Id"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State++"_min"))}
	end, states()) ++ [
		#dcell {type=header, val="Total"}
	],



	%
	% return
	%
	{
		D#dig {},
		[Header] ++ dig:append_total_cells(Results)
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
	AllEligibleProfileIds = ProfileIds ++ anpcandidates:get_evaluators_for_test(TFs, anpevaluator),
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
		% get stats per profile
		%
		[
			#dcell {val=itl:blockquote([
				itf:val(ProfileDoc, fullname),
				itf:val(ProfileDoc, mobile),
				itf:val(ProfileDoc, email)
			])}
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
		end, states())

	end, ["unassigned"] ++ ProfileIdsUnique),



	%
	% header
	%
	Header = [
		#dcell {type=header, val="Profile"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State++"_min"))}
	end, states()) ++ [
		#dcell {type=header, val="Total"}
	],




	%
	% return
	%
	{
		D#dig {
			total=length(ProfileDocs)
		},
		[Header] ++ dig:append_total_cells(Results)
	};


%..............................................................................
%
% [other]
%
%..............................................................................
fetch(D, _From, _Size, _) ->
	{
		D,
		[{error, "This combination of filters has not been implemented.
		If you think it is useful, please contact the support team."}]
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
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


%
% states
%
states() ->
	lists:map(fun(State) ->
		?A2L(State)
	end, helper_options:options(anpstate)).



%
% get class
%
get_class(State, Number) when Number > 0 ->
	case State of
		"anpstate_not_uploaded" ->
			"bg-info";
		"anpstate_yettostart" ->
			"bg-warning";
		"anpstate_active" ->
			"bg-danger";
		"anpstate_completed" ->
			"bg-success";
		"anpstate_moderation" ->
			"bg-danger";
		"anpstate_moderation_completed" ->
			"bg-success";
		"anpstate_revaluation" ->
			"bg-danger";
		"anpstate_revaluation_completed" ->
			"bg-success";
		"anpstate_moderation_reval" ->
			"bg-danger";
		"anpstate_moderation_reval_completed" ->
			"bg-success";
		"anpstate_evaluation_rejected" ->
			"bg-danger";
		"anpstate_discarded" ->
			"bg-info"
	end;


get_class(_, _) ->
	[].




%
% get evaluation count
%
get_eval_count_for_profile(_, error, _, _) ->
	0;
get_eval_count_for_profile("unassigned", {ok, Val}, State, _) when
	State == "anpstate_not_uploaded";
	State == "anpstate_yettostart";
	State == "anpstate_discarded" ->
	Val;
get_eval_count_for_profile(_, {ok, Val}, State, "anpevaluator") when
	State == "anpstate_yettostart";
	State == "anpstate_active";
	State == "anpstate_completed";
	State == "anpstate_evaluation_rejected" ->
	Val;
get_eval_count_for_profile(_, {ok, Val}, State, "anpmoderator") when
	State == "anpstate_moderation";
	State == "anpstate_moderation_completed" ->
	Val;
get_eval_count_for_profile(_, {ok, Val}, State, "anprevaluator") when
	State == "anpstate_revaluation";
	State == "anpstate_revaluation_completed" ->
	Val;
get_eval_count_for_profile(_, {ok, Val}, State, "anpmoderator_reval") when
	State == "anpstate_moderation_reval";
	State == "anpstate_moderation_reval_completed" ->
	Val;
get_eval_count_for_profile(_, _, _, _) ->
	0.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
