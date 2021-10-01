
-module(dig_ep_osm_bundle_stats).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("Scanning Statistics").

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
			?COREXS(season_fk)
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("Scanning Statistics - Number of booklets").



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
fetch(D, _From, _Size, [
	]) ->

	%
	% init
	%
	Stats = ep_osm_bundle_api:get_stats(),
	StatsDict = dict:from_list(Stats),


	%
	% season ids
	%
	SeasonIds = lists:map(fun({[_, SeasonId], _}) ->
		SeasonId
	end, Stats),
	SeasonIdsUnique = helper:unique(SeasonIds),
	SeasonDocs = ep_core_exam_season_api:getdocs_by_ids(SeasonIdsUnique),


	%
	% layout
	%
	Results = lists:map(fun(SeasonDoc0) ->

		SeasonDoc = case SeasonDoc0 of
			undefined ->
				{[{<<"_id">>, <<"unassigned">>}]};
			_ ->
				SeasonDoc0
		end,
		SeasonId = itf:idval(SeasonDoc),
		[
			#dcell {
				val=itl:blockquote([
					itf:val(SeasonDoc, name),
					itf:val(SeasonDoc, state)
				]),
				postback={filter, itf:build(?COREXS(season_fk), SeasonId)}
			}
		] ++ lists:map(fun(State) ->
			Val = case dict:find([State, SeasonId], StatsDict) of
				{ok, Count} ->
					Count;
				error ->
					0
			end,
			dig:if_not(0, info, #dcell {val=Val})
		end, states())
	end, SeasonDocs),


	%
	% sort results
	%
	ResultsSorted = lists:sort(fun(A, B) ->
		#dcell {val=Val1} = lists:nth(2, A),
		#dcell {val=Val2} = lists:nth(2, B),
		Val1 > Val2
	end, Results),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Season"},
		#dcell {type=header, val="Inward Completed"},
		#dcell {type=header, val="Scanning Completed"},
		#dcell {type=header, val="Upload Completed"},
		#dcell {type=header, val="Total"}
	],


	%
	% return
	%
	{
		D#dig {
			total=length(ResultsSorted)
		},
		[Header] ++ dig:append_total_cells(ResultsSorted)
	};



%..............................................................................
%
% [season_fk]
%
%..............................................................................
fetch(D, _From, _Size, [
	#field {id=season_fk, uivalue=SeasonId0}
]) ->

	%
	% init
	%
	Today = helper:date_today_str(),
	TodaySeconds = helper:date_d2epoch(Today),
	Stats = ep_osm_bundle_api:get_stats(SeasonId0),
	StatsDict = dict:from_list(Stats),


	%
	% season exam ids
	%
	SeasonExamIds = lists:map(fun({[_, SeasonId, ExamId], _}) ->
		{SeasonId, ExamId}
	end, Stats),
	SeasonExamIdsUnique = helper:unique(SeasonExamIds),


	%
	% exam docs dict
	%
	ExamIds = lists:map(fun({_, ExamId}) ->
		ExamId
	end, SeasonExamIdsUnique),
	ExamDocs = ep_osm_exam_api:getdocs_by_ids(ExamIds),
	ExamDocsDict = helper:get_dict_from_docs(ExamDocs),


	%
	% layout
	%
	Results = lists:map(fun({SeasonId, ExamId}) ->
		{ok, ExamDoc} = dict:find(ExamId, ExamDocsDict),
		[
			#dcell {
				val=#link {
					new=true,
					body=itl:blockquote([
						itf:val(ExamDoc, anptestcourseid),
						itf:val(ExamDoc, testname)
					]),
					url=io_lib:format("/dig_ep_osm_exam_inward?id=~s", [ExamId])
				}
			},
			dig_ep_osm_exam_evaluation_stats:dcell_days_since_test(TodaySeconds, ExamDoc)
		] ++ lists:map(fun(State) ->
			Val = case dict:find([State, SeasonId, ExamId], StatsDict) of
				{ok, Count} ->
					Count;
				error ->
					0
			end,
			dig:if_not(0, info, #dcell {val=Val})
		end, states())
	end, SeasonExamIdsUnique),


	%
	% sort results
	%
	ResultsSorted = lists:sort(fun(A, B) ->
		#dcell {val=Val1} = lists:nth(3, A),
		#dcell {val=Val2} = lists:nth(3, B),
		Val1 > Val2
	end, Results),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Exam"},
		#dcell {type=header, val="Days"},
		#dcell {type=header, val="Inward Completed"},
		#dcell {type=header, val="Scanning Completed"},
		#dcell {type=header, val="Upload Completed"},
		#dcell {type=header, val="Total"}
	],


	%
	% return
	%
	{
		D#dig {
			total=length(ResultsSorted)
		},
		[Header] ++ dig:append_total_cells(ResultsSorted)
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

states() -> [
	"inward_completed",
	"scanning_completed",
	"upload_completed"
].


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
