
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
			?COREXS({season_fk, ?ACTIVE}),
			?CORFAC(faculty_code_fk),
			?CORPGM(program_code_fk),
			?CORSUB(subject_code_fk),
			fields:get(anptestcourseid),
			fields:get(teststatus),
			fields:get(exam_pattern),
			itf:build(itf:hidden(osm_exam_fk), minijobcontext:q(osm_exam_fk_)),
			itf:build(itf:hidden(group), minijobcontext:q(group_))
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
% [osm_exam_fk] ip group
%
%..............................................................................

fetch(D, _From, _Size, [
	#field {id=profileid, uivalue=ProfileId},
	#field {id=role, uivalue=Role},
	#field {id=osm_exam_fk, uivalue=TestId},
	#field {id=group, uivalue="ip"}
]) ->

	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(TestId),
	UserIPs = get_user_ips(ProfileId, Role),


	%
	% get stats
	%
	Stats = ep_osm_exam_api:get_capcentre_stats_test(TestId, UserIPs, 2),
	StatsDict = lists:foldl(fun({[IP, State], Count}, AccDict) ->
		dict:update_counter([IP, State], Count, AccDict)
	end, dict:new(), Stats),


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
			#dcell {type=label, val=IP},
			#dcell {type=label, val=CapCentreName}
		] ++ lists:map(fun(State) ->
			Key = [IP, State],
			case dict:find(Key, StatsDict) of
				{ok, Val} ->
					#dcell {bgcolor=?BGCOLOR_INFO, val=Val};
				_ ->
					#dcell {val=0}
			end
		end, states())

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
		#dcell {type=header, val="Total"}
	],



	{
		D#dig {
			total=length(Results),
			description=itx:format("~s / ~s", [
				itf:val(ExamDoc, anptestcourseid), itf:val(ExamDoc, testname)
			]),
			show_filter=false
		},
		[Header | dig:append_total_cells(Results)]
	};




%..............................................................................
%
% [osm_exam_fk] date group
%
%..............................................................................

fetch(D, _From, _Size, [
	#field {id=profileid, uivalue=ProfileId},
	#field {id=role, uivalue=Role},
	#field {id=osm_exam_fk, uivalue=TestId},
	#field {id=group, uivalue="date"}
]) ->

	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(TestId),
	UserIPs = get_user_ips(ProfileId, Role),


	%
	% get stats
	%
	Stats = ep_osm_exam_api:get_capcentre_stats_test(TestId, UserIPs, 3),
	StatsDict = lists:foldl(fun({[_IP, State, Date], Count}, AccDict) ->
		dict:update_counter([Date, State], Count, AccDict)
	end, dict:new(), Stats),


	%
	% get unique ips
	%
	Keys = dict:fetch_keys(StatsDict),
	Dates = lists:map(fun([Date, _State]) ->
		Date
	end, Keys),
	Dates1 = helper:sort(helper:unique(Dates)),


	%
	% results
	%
	Results = lists:map(fun(Date) ->
		[
			#dcell {type=label, val=Date}
		] ++ lists:map(fun(State) ->
			Key = [Date, State],
			case dict:find(Key, StatsDict) of
				{ok, Val} ->
					#dcell {bgcolor=?BGCOLOR_INFO, val=Val};
				_ ->
					#dcell {val=0}
			end
		end, states())

	end, Dates1),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Date"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State))}
	end, states()) ++ [
		#dcell {type=header, val="Total"}
	],



	{
		D#dig {
			total=length(Results),
			description=itx:format("~s / ~s", [
				itf:val(ExamDoc, anptestcourseid), itf:val(ExamDoc, testname)
			]),
			show_filter=false
		},
		[Header | dig:append_total_cells(Results)]
	};




%..............................................................................
%
% [osm_exam_fk] evaluator group
%
%..............................................................................

fetch(D, _From, _Size, [
	#field {id=profileid, uivalue=ProfileId},
	#field {id=role, uivalue=Role},
	#field {id=osm_exam_fk, uivalue=TestId},
	#field {id=group, uivalue="evaluator"}
]) ->
	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(TestId),
	UserIPs = get_user_ips(ProfileId, Role),


	%
	% get stats
	%
	Stats = ep_osm_exam_api:get_capcentre_stats_test(TestId, UserIPs, 4),
	StatsDict = dict:from_list(Stats),
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
		[IPFn, _State, _Date, ProfileIdFn],
		{AccProfileIds, AccIPs}) ->
		{
			AccProfileIds ++ [ProfileIdFn],
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
			#dcell {type=label, val=IPx},
			#dcell {type=label, val=CapCentreName},
			#dcell {type=label, val=Datex},
			#dcell {type=label, val=?LN(?L2A(Statex))},
			#dcell {type=label, val=itf:val(ProfileDoc, fullname)},
			#dcell {type=label, val=itf:val(ProfileDoc, mobile)},
			#dcell {type=label, val=itf:val(ProfileDoc, email)},
			#dcell {type=label, val=?LN(?L2A(itf:val(ProfileDoc, profiletype)))},
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
		#dcell {type=header, val="Count"},
		#dcell {type=header, val="Total"}
	],



	{
		D#dig {
			total=length(Results),
			description=itx:format("~s / ~s", [
				itf:val(ExamDoc, anptestcourseid), itf:val(ExamDoc, testname)
			]),
			show_filter=false
		},
		[Header | dig:append_total_cells(Results)]
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
			#dcell {type=label, val=itl:render(F)}
		end, itf:d2f(Doc, fs(anptest))),


		%
		% get stats
		%
		TestId = itf:idval(Doc),
		Stats = ep_osm_exam_api:get_capcentre_stats_test(TestId, IPs, 2),
		StatsDict = lists:foldl(fun({[_, State], Count}, AccDict) ->
			dict:update_counter(State, Count, AccDict)
		end, dict:new(), Stats),


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
				type=label,
				val_export=[],
				val=get_links(Doc)
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
		#dcell {type=header, val="Actions"},
		#dcell {type=header, val="Total"}
	],


	{D#dig {total=?INFINITY}, [Header | dig:append_total_cells(Results)]}.


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



get_links(Doc) ->

	TestId = itf:idval(Doc),
	Groups = [
		{ip, "IPs"},
		{date, "Dates"},
		{evaluator, "Evaluators"}
	],

	Links = lists:map(fun({Group, Label}) ->
		#link {
			text=Label,
			url=itx:format("/~p?osm_exam_fk_=~s&group_=~p", [
				?MODULE, TestId, Group
			])
		}
	end, Groups),

	akit_dropdown:button_group("", Links).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
