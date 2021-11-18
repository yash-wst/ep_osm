
-module(ep_osm_candidate_api).
-compile(export_all).
-include("records.hrl").

db() ->
	throw(does_not_exist).

db(Id) ->
	anpcandidates:db(Id).

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
	fetch(anptestid(), From, Size, Filters).

fetch(ExamId, From, Size, []) ->
	anpcandidates:getdocs(db(ExamId), From, Size);

fetch(ExamId, From, Size, Filters) ->
	fetch(ExamId, From, Size, Filters, []).

fetch(ExamId, From, Size, Filters, Configs) ->
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		db(ExamId), Filters, From, Size, Configs
	),
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

anptestid() ->
	wf:q(anptest:id()).

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

