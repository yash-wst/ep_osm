
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
		#dcell {type=header, val="Upload Completed"}
	],


	%
	% return
	%
	{
		D#dig {
			total=length(Results)
		},
		[Header] ++ Results
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
