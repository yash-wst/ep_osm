
-module(ep_osm_mscheme_api).
-compile(export_all).
-include("records.hrl").

db() ->
	"ep_osm_mscheme".

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


	%
	% asserts
	%
	assert_doc_not_published(Doc),


	FsAll1 = itf:d2f(Doc, FsAll),


	%
	% get changelist
	%
	Changelist = itf:fs_changelist(Doc, FsToSave),
	FNotes = itf:d2f(Doc, ?OSMMSC(notes)),
	FsToSave1 = FsToSave ++ [
		itf:build_comment(FNotes, Changelist)
	],


	FsAll2 = itf:fs_merge(FsAll1, FsToSave1),
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
% 
%------------------------------------------------------------------------------

assert_doc_not_published(Doc) ->
	?ASSERT(
		itf:val(Doc, state) /= "published",
		"ERROR! Editing of published documents is not allowed!"
	).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

