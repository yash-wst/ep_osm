-module(ep_osm_setup).
-compile(export_all).
-include("records.hrl").


init() ->
	dbs(),
	views().


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
	].
