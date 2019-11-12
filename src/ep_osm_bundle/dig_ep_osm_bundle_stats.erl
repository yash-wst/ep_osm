
-module(dig_ep_osm_bundle_stats).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

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
	?LN("Scanning Statistics").



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


	%
	% layout
	%
	Results = lists:map(fun(SeasonId) ->
		[
			#dcell {
				val=SeasonId,
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
	end, SeasonIdsUnique),


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
			total=length(Results)
		},
		[Header] ++ dig:append_total_cells(Results)
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
	% layout
	%
	Results = lists:map(fun({SeasonId, ExamId}) ->
		[
			#dcell {
				val=ExamId,
				postback={filter, itf:build(?OSMBDL(osm_exam_fk), ExamId)}
			}
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
			total=length(Results)
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

states() -> [
	"inward_completed",
	"scanning_completed",
	"upload_completed"
].


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
