
-module(ep_osm_bundle_api).
-compile(export_all).
-include("records.hrl").

db() ->
	"ep_osm_bundle".

%------------------------------------------------------------------------------
% get
%------------------------------------------------------------------------------
get(Id) ->
	db:get(db(), Id).

%
% assuming a view for the specified field exists
%
get_by_field(F = #field {}) ->
	itxview:find_docs(db(), F).



getdocs() ->
	db:getdocs(db()).


list() ->
	lists:map(fun(D) ->
		{helper:l2a(itf:idval(D)), itf:idval(D)}
	end, db:getdocs(db())).


%
% get count by exam id
%
get_count_by_osm_exam_fk(ExamId) ->

	%
	% init
	%
	Fs = [
		itf:build(?OSMBDL(osm_exam_fk), ExamId)
	],
	ViewName = {"osm_exam_fk", "osm_exam_fk"},
	SK = itxview:fields_to_sk(Fs),
	EK = itxview:fields_to_ek(Fs),


	%
	% count
	%
	itxview:get_count_by_fields1(db(), SK, EK, ViewName).



%------------------------------------------------------------------------------
% check
%------------------------------------------------------------------------------
exists(F = #field {}) ->
	{Count, _} = get_by_field(F),
	Count > 0.



%------------------------------------------------------------------------------
% create
%------------------------------------------------------------------------------
create(Fs) ->
	%
	% execute create in queue to avoid duplicating bundle numbers
	%
	mini_task_queue:now(fun create1/1, Fs).


create1(Fs) ->

	%
	% init
	%
	ExamId = itf:val2(Fs, osm_exam_fk),
	BundleNumber = itf:val2(Fs, number),
	FOsmExam = itf:build(?OSMBDL(osm_exam_fk), ExamId),


	%
	% get next bundle number
	%
	NextBundleNumber = case BundleNumber of
		BundleNumber when BundleNumber == []; BundleNumber == undefined ->
			#db2_find_response {docs=Docs} = db2_find:get_by_fs(
				db(), [FOsmExam], 0, ?INFINITY
			),
			?I2S(length(Docs) + 1);
		_ ->
			BundleNumber
	end,


	%
	% merge fields
	%
	FsCreate = Fs ++ [
		itf:build(?OSMBDL(number), NextBundleNumber)
	],



	%
	% save
	%
	?MODULE:save(FsCreate).



%------------------------------------------------------------------------------
% save
%------------------------------------------------------------------------------
save(Fields) ->
	db:save(db(), helper_api:fields2doc(Fields)).

save(FsToSave, FsAll, Id) ->
	{ok, Doc} = ?MODULE:get(Id),
	FsAll1 = itf:d2f(Doc, FsAll),
	FsAll2 = itf:fs_merge(FsAll1, FsToSave),
	?MODULE:save(FsAll2).

savebulk(LoLofFields) ->
	Docs = lists:map(fun(Fs) ->
		helper_api:fields2doc(Fs)
	end, LoLofFields),

	db:savebulk(db(), Docs).


%------------------------------------------------------------------------------
% mutate
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fetch
%------------------------------------------------------------------------------

fetch(From, Size, Filters) ->
	fetch(From, Size, Filters, []).
fetch(From, Size, Filters, Config) ->
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(db(), Filters, From, Size, Config),
	Docs.


%------------------------------------------------------------------------------
% delete
%------------------------------------------------------------------------------
delete_by_field(F = #field {}) ->
	{_, Docs} = get_by_field(F),
	lists:map(fun(D) ->
		db:delete_id(db(), itf:idval(D))
	end, Docs).


%------------------------------------------------------------------------------
% stats
%------------------------------------------------------------------------------

get_stats() ->
	SK = [<<"">>, <<"">>, <<"">>],
	EK = [<<"z\\ufff0">>, <<"z\\ufff0">>, <<"z\\ufff0">>],
	itxview:get_stats(
		db(), "bundle_state_season_fk_osm_exam_fk", SK, EK, 2
	).


get_stats(SeasonId) ->
	lists:foldl(fun(State, Acc) ->
		SK = [?L2B(State), ?L2B(SeasonId), <<"">>],
		EK = [?L2B(State), ?L2B(SeasonId), <<"z\\ufff0">>],
		Acc ++ itxview:get_stats(
			db(), "bundle_state_season_fk_osm_exam_fk", SK, EK, 3
		)
	end, [], dig_ep_osm_bundle_stats:states()).



get_stats_of_active_seasons() ->

	%
	% get active seasons
	%
	ActiveSeasonIds = lists:map(fun({Id, _Name}) ->
		Id
	end, ep_core_exam_season_api:list_active()),


	%
	% get stats
	%
	Stats = ep_osm_bundle_api:get_stats(),


	%
	% acc stats
	%
	Dict = lists:foldl(fun({[State, SeasonId], Count}, Acc) ->
		case lists:member(SeasonId, ActiveSeasonIds) of
			true ->
				dict:update_counter(State, Count, Acc);
			false ->
				Acc
		end
	end, dict:new(), Stats),


	%
	% return
	%
	dict:to_list(Dict).






%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%..............................................................................
%
% update - bundle_size
%
%..............................................................................

update_bundle_size() ->

	%
	% get all bundles in inward completed state
	%
	FsFind = [
		itf:build(?OSMBDL(inwardstate), "completed")
	],
	#db2_find_response {docs=BundleDocs} = db2_find:get_by_fs(
		db(), FsFind, 0, ?INFINITY
	),


	%
	% get candidate docs for this bundle and get size
	%
	LoLofFields = lists:map(fun(BundleDoc) ->

		%
		% get candidate docs for this bundle
		%
		CandidateDocs = dig_ep_osm_exam_inward:get_bundle_docs(
			itf:val(BundleDoc, osm_exam_fk), itf:idval(BundleDoc)
		),


		%
		% new fs
		%
		FsToSave = [
			itf:build(?OSMBDL(bundle_size), ?I2S(length(CandidateDocs)))
		],
		FsAll = itf:d2f(BundleDoc, ep_osm_bundle:fs(all)),
		itf:fs_merge(FsAll, FsToSave)


	end, BundleDocs),



	%
	% save
	%
	ep_osm_bundle_api:savebulk(LoLofFields).



%..............................................................................
%
% update - season_fk
%
%..............................................................................

update_bundle_season_fk() ->

	%
	% get all bundles
	%
	#db2_find_response {docs=BundleDocs} = db2_find:get_by_fs(
		db(), [], 0, ?INFINITY
	),


	%
	% get all exam docs
	%
	ExamIds = lists:map(fun(BundleDoc) ->
		itf:val(BundleDoc, osm_exam_fk)
	end, BundleDocs),
	?D(ExamIds),
	ExamDocs = ep_osm_exam_api:getdocs_by_ids(ExamIds),
	ExamDocsDict = helper:get_dict_from_docs(ExamDocs),



	LoLofFields = lists:map(fun(BundleDoc) ->


		%
		% init
		%
		{ok, ExamDoc} = dict:find(itf:val(BundleDoc, osm_exam_fk), ExamDocsDict),


		%
		% new fs
		%
		FsToSave = [
			itf:build(?COREXS(season_fk), itf:val(ExamDoc, season_fk))
		],
		FsAll = itf:d2f(BundleDoc, ep_osm_bundle:fs(all)),
		itf:fs_merge(FsAll, FsToSave)

	end, BundleDocs),


	%
	% save
	%
	ep_osm_bundle_api:savebulk(LoLofFields).




%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

