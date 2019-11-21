-module(ep_osm_setup).
-compile(export_all).
-include("records.hrl").


init() -> [
	dbs(),
	db2indices()
].


%
% create all databases and views required by ep_core
%
dbs() ->
	lists:map(fun(Db) ->
		db:create_db(Db)
	end, dblist()).


dbs(delete) ->
	lists:map(fun(Db) ->
		db:delete_db(Db)
	end, dblist()).


%
% db2indices
%
db2indices() ->
	lists:map(fun({Db, Fs}) ->
		Fs1 = [
			itf:id()
		] ++ Fs,
		db2_index:create(Db, Fs1)
	end, db2indiceslist()).



%
% create all views required by ep_core
%
views() ->
	lists:map(fun(Db) ->
		ViewDir = view:couchdb_views_dir(ep_osm, Db),
		view:setup_views(Db, ViewDir)
	end, dblist()).


views(delete) ->
	lists:map(fun(Db) ->
		db:delete_id(Db, "_design/" ++ Db)
	end, dblist()).


%
% list dbs
%
dblist() ->
	[
		"ep_osm_cap",
		"ep_osm_bundle",
		"ep_osm_mscheme"
	].



%
% list db2 indices
%
db2indiceslist() -> [
	{"ep_osm_cap", ep_osm_cap:fs(index)},
	{"ep_osm_bundle", ep_osm_bundle:fs(index)},
	{"ep_osm_mscheme", ep_osm_mscheme:fs(index)}
].



viewlist() -> [
].
