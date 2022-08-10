
-module(ep_osm_cap_api).
-compile(export_all).
-include("records.hrl").

db() ->
	"ep_osm_cap".

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



getdocs_dict_by_ip(IPs) ->
	IPsUnique = helper:unique(IPs),
	FsFinCap = [
		db2es_find:get_field_cond("$in", ips, IPsUnique)
	],
	CapCentreDocs = ep_osm_cap_api:fetch(0, ?INFINITY, FsFinCap),
	CapCentreDocsIPList = lists:foldl(fun(CapDoc, Acc) ->
		Acc ++ lists:map(fun(IP) ->
			{IP, CapDoc}
		end, itf:val(CapDoc, ?OSMCAP(ips)))
	end, [], CapCentreDocs),
	dict:from_list(CapCentreDocsIPList).


list() ->
	lists:map(fun(D) ->
		{helper:l2a(itf:idval(D)), itf:idval(D)}
	end, db:getdocs(db())).


%------------------------------------------------------------------------------
% check
%------------------------------------------------------------------------------
exists(F = #field {}) ->
	{Count, _} = get_by_field(F),
	Count > 0.


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
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(db(), Filters, From, Size),
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
% misc
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

