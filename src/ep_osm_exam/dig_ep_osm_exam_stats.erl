
-module(dig_ep_osm_exam_stats).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Exam Statistics").

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
			?OSMEXM(osm_exam_fk)
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exam Statistics").



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
	] = Fs) ->

	%
	% get stats
	%
	Stats = ep_osm_exam_api:get_stats(Fs),
	StatsDict = dict:from_list(Stats),


	%
	% layout
	%
	SeasonDocs = ep_core_exam_season_api:getdocs(),
	Results = lists:map(fun(SeasonDoc) ->
		SeasonId = itf:idval(SeasonDoc),
		[
			#dcell {
				val=itl:blockquote([
					itf:val(SeasonDoc, name),
					itf:val(SeasonDoc, state)
				]),
				postback={filter, [
					itf:build(?COREXS(season_fk), SeasonId)
				]}
			}
		] ++ lists:map(fun(State) ->
			Val = case dict:find([State, SeasonId], StatsDict) of
				{ok, Count} ->
					Count;
				_ ->
					0
				end,
			dig:if_not(0, primary, #dcell {val=Val})
		end, ep_osm_exam_api:states())
	end, SeasonDocs),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Season"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State))}
	end, ep_osm_exam_api:states()),


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
% [season_fk]
%
%..............................................................................
fetch(D, _From, _Size, [
	#field {id=season_fk, uivalue=SeasonId}
] = Fs) ->

	%
	% get stats
	%
	Stats = ep_osm_exam_api:get_stats(Fs),
	StatsDict = dict:from_list(Stats),


	%
	% get faculty docs
	%
	FacultyIds = lists:map(fun({[_, _, FacultyId], _}) ->
		FacultyId
	end, Stats),
	FacultyIdsUnique = helper:unique(FacultyIds),
	FacultyDocs0 = ep_core_faculty_api:getdocs_by_ids(FacultyIdsUnique),
	FacultyDocs = lists:map(fun(FacultyDoc) ->
		case FacultyDoc of
			undefined ->
				{[]};
			_ ->
				FacultyDoc
		end
	end, FacultyDocs0),


	%
	% layout
	%
	Results = lists:map(fun(FacultyDoc) ->
		FacultyId = itf:idval(FacultyDoc),
		[
			#dcell {
				val=itl:blockquote([
					itf:val(FacultyDoc, faculty_code),
					itf:val(FacultyDoc, faculty_name)
				]),
				postback={filter, [
					itf:build(?COREXS(season_fk), SeasonId),
					itf:build(?CORFAC(faculty_code_fk), FacultyId)
				]}
			}
		] ++ lists:map(fun(State) ->
			Val = case dict:find([State, SeasonId, FacultyId], StatsDict) of
				{ok, Count} ->
					Count;
				_ ->
					0
				end,
			dig:if_not(0, primary, #dcell {val=Val})
		end, ep_osm_exam_api:states())
	end, FacultyDocs),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Faculty"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State))}
	end, ep_osm_exam_api:states()),


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



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
