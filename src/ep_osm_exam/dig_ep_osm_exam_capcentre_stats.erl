
-module(dig_ep_osm_exam_capcentre_stats).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("CAP Centre Stats").

heading() ->
	title().

-define(BGCOLOR_INFO, "table-info").


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_CAPADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% field
%------------------------------------------------------------------------------

fs(anptest) ->
	fields:getfields([
		anptestcourseid,
		testname
	]).


%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=[
			itf:build(itf:hidden(profileid), itxauth:profileid()),
			itf:build(itf:hidden(role), itxauth:role()),
			?COREXS(season_fk),
			?CORFAC(faculty_code_fk),
			?CORPGM(program_code_fk),
			?CORSUB(subject_code_fk),
			fields:get(anptestcourseid),
			fields:get(teststatus),
			fields:get(exam_pattern),
			itf:build(itf:hidden(osm_exam_fk), minijobcontext:q(osm_exam_fk_)),
			itf:build(itf:hidden(ip), minijobcontext:q(ip_))
		],
		events=[
			ite:button(export, "CSV", {itx, {dig, export}})
		],
		config=[
			{responsive_type, scroll}
		],
		size=25
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	title().



%------------------------------------------------------------------------------
% function - init
%------------------------------------------------------------------------------
init() ->
	ok.


%------------------------------------------------------------------------------
% function - fetch
%------------------------------------------------------------------------------

fetch(D, From, Size, [
	#field {id=profileid},
	#field {id=role} | []
] = Fs) ->
	fetch(D, From, Size, Fs ++ [
		fields:build(teststatus, ?ACTIVE)
	]);


%..............................................................................
%
% [osm_exam_fk, ip]
%
%..............................................................................

fetch(D, _From, _Size, [
	#field {id=profileid},
	#field {id=role},
	#field {id=osm_exam_fk, uivalue=TestId},
	#field {id=ip, uivalue=IP}
]) ->
	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(TestId),


	%
	% get stats
	%
	StatsDict = ep_osm_exam_api:get_capcentre_stats_ip(TestId, IP),
	Stats = dict:to_list(StatsDict),
	StatsSorted = lists:sort(fun(A, B) ->
		{[_, _, Date1, _], _} = A,
		{[_, _, Date2, _], _} = B,
		DA = helper:date_s2d(Date1),
		DB = helper:date_s2d(Date2),
		DA < DB
	end, Stats),



	%
	% get unique ips, profileids, dates
	%
	Keys = dict:fetch_keys(StatsDict),
	{ProfileIds, IPs} = lists:foldl(fun(
		[IPFn, _State, _Date, ProfileId],
		{AccProfileIds, AccIPs}) ->
		{
			AccProfileIds ++ [ProfileId],
			AccIPs ++ [IPFn]
		}
	end, {[], []}, Keys),


	%
	% get profile docs dict
	%
	ProfileIdsUnique = helper:unique(ProfileIds),
	ProfileDocs = profiles:getdocs_by_ids(ProfileIdsUnique),
	ProfileDocsDict = helper:get_dict_from_docs(ProfileDocs),


	%
	% get cap centre dict
	%
	CapCentreDocsDict = ep_osm_cap_api:getdocs_dict_by_ip(IPs),



	%
	% results
	%
	Results = lists:map(fun({[IPx, Statex, Datex, ProfileIdx], Count}) ->

		{ok, ProfileDoc} = dict:find(ProfileIdx, ProfileDocsDict),
		CapCentreName = case dict:find(IPx, CapCentreDocsDict) of
			{ok, CapDoc} ->
				itf:val(CapDoc, name);
			_ ->
				IPx
		end,

		[
			#dcell {val=IPx},
			#dcell {val=CapCentreName},
			#dcell {val=Datex},
			#dcell {val=?LN(?L2A(Statex))},
			#dcell {val=itf:val(ProfileDoc, fullname)},
			#dcell {val=itf:val(ProfileDoc, mobile)},
			#dcell {val=itf:val(ProfileDoc, email)},
			#dcell {val=?LN(?L2A(itf:val(ProfileDoc, profiletype)))},
			#dcell {val=Count}
		]
	end, StatsSorted),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="IP Address"},
		#dcell {type=header, val="CAP Centre"},
		#dcell {type=header, val="Date Submitted"},
		#dcell {type=header, val="Evaluation State"},
		#dcell {type=header, val="Full name"},
		#dcell {type=header, val="Mobile"},
		#dcell {type=header, val="E-mail"},
		#dcell {type=header, val="Role"},
		#dcell {type=header, val="Count"}
	],



	{
		D#dig {
			description=itx:format("~s / ~s", [
				itf:val(ExamDoc, anptestcourseid), itf:val(ExamDoc, testname)
			]),
			show_filter=false
		},
		[Header | Results]
	};



%..............................................................................
%
% [osm_exam_fk]
%
%..............................................................................

fetch(D, _From, _Size, [
	#field {id=profileid},
	#field {id=role},
	#field {id=osm_exam_fk, uivalue=TestId}
]) ->

	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(TestId),


	%
	% get stats
	%
	StatsDict = ep_osm_exam_api:get_capcentre_stats_test(TestId),


	%
	% get unique ips
	%
	Keys = dict:fetch_keys(StatsDict),
	IPs = lists:map(fun([IP, _]) ->
		IP
	end, Keys),
	IPs1 = helper:sort(helper:unique(IPs)),


	%
	% get cap centre dict
	%
	CapCentreDocsDict = ep_osm_cap_api:getdocs_dict_by_ip(IPs),



	%
	% results
	%
	Results = lists:map(fun(IP) ->

		CapCentreName = case dict:find(IP, CapCentreDocsDict) of
			{ok, CapDoc} ->
				itf:val(CapDoc, name);
			_ ->
				IP
		end,


		[
			#dcell {val=IP},
			#dcell {val=CapCentreName}
		] ++ lists:map(fun(State) ->
			Key = [IP, State],
			case dict:find(Key, StatsDict) of
				{ok, Val} ->
					#dcell {bgcolor=?BGCOLOR_INFO, val=Val};
				_ ->
					#dcell {val=0}
			end
		end, states()) ++ [
			#dcell {
				val_export=[],
				val=#link {
					text="View",
					url=itx:format("/~p?osm_exam_fk_=~s&ip_=~s", [
						?MODULE, TestId, IP
					])
				}
			}
		]

	end, IPs1),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="IP"},
		#dcell {type=header, val="CAP Centre"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State))}
	end, states()) ++ [
		#dcell {type=header, val="Action"}
	],



	{
		D#dig {
			description=itx:format("~s / ~s", [
				itf:val(ExamDoc, anptestcourseid), itf:val(ExamDoc, testname)
			]),
			show_filter=false
		},
		[Header | Results]
	};




%..............................................................................
%
% Any
%
%..............................................................................
fetch(D, From, Size, [
	#field {id=profileid, uivalue=ProfileId},
	#field {id=role, uivalue=Role} | Fs
]) ->

	%
	% init
	%
	Docs = ep_osm_exam_api:fetch(From, Size, Fs, [
		{use_index, ["season_fk"]}
	]),
	{SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict} =
		ep_core_helper:get_sfps_dicts(Docs),
	IPs = get_user_ips(ProfileId, Role),




	%
	% get results
	%
	Results = lists:map(fun(Doc) ->

		%
		% spf cells
		%
		SFPSCells = ep_core_dig_helper:get_sfps_cells(
			Doc, {SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict},
			#dcell {show_ui=false}
		),


		%
		% test cells
		%
		ExamCells = lists:map(fun(F) ->
			#dcell {val=itl:render(F)}
		end, itf:d2f(Doc, fs(anptest))),


		%
		% get stats
		%
		TestId = itf:idval(Doc),
		StatsDict = ep_osm_exam_api:get_capcentre_stats_dashboard(TestId, IPs),
		StatsCells = lists:map(fun(State) ->
			case dict:find(State, StatsDict) of
				{ok, Val} ->
					#dcell {bgcolor=?BGCOLOR_INFO, val=Val};
				_ ->
					#dcell {val=0}
			end
		end, states()),


		%
		% action cells
		%
		ActionCells = [
			#dcell {
				val_export=[],
				val=#link {
					text="View",
					url=itx:format("/~p?osm_exam_fk_=~s", [
						?MODULE, TestId
					])
				}
			}
		],


		%
		% return
		%
		SFPSCells ++ ExamCells ++ StatsCells ++ ActionCells


	end, Docs),


	Header = ep_core_dig_helper:get_sfps_cells_header() ++
	lists:map(fun(#field {label=Label}) ->
		#dcell {type=header, val=Label}
	end, fs(anptest)) ++ 
	lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State))}
	end, states()) ++ [
		#dcell {type=header, val="Actions"}
	],


	{D#dig {}, [Header | Results]}.


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
	"anpstate_completed",
	"anpstate_moderation_completed",
	"anpstate_revaluation_completed",
	"anpstate_moderation_reval_completed"
].


get_user_ips(_ProfileId, ?APPOSM_ADMIN) ->
	all;
get_user_ips(ProfileId, ?APPOSM_CAPADMIN) ->

	%
	% init
	%
	{ok, PDoc} = ep_osm_cap_admin_api:get(ProfileId),
	{ok, CapDoc} = ep_osm_cap_api:get(itf:val(PDoc, osm_cap_fk)),
	itf:val(CapDoc, ?OSMCAP(ips)).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
