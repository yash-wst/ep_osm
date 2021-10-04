-module(ep_osm_mod_rules).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("OSM Moderation Rules").

heading() ->
	?LN("OSM Moderation Rules").

db() ->
	ep_osm_mod_rules_api:db().

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, _) -> false.

%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

fs(basic) -> [
	?OSMRLS(name),
	?OSMRLS(type),
	?OSMRLS(rules)
];

fs(index) -> [
	?OSMRLS(name),
	?OSMRLS(type)
];

fs(view) ->
	fs(basic);

fs(create) -> [
	?OSMRLS(name),
	?OSMRLS(type)
];

fs(edit) ->
	fs(basic);

fs({edit, Doc}) -> [
	?OSMRLS(name),
	?OSMRLS(type),
	?OSMRLS({rules, Doc})
];

fs(update) ->
	fs(basic);

fs(search) -> [
	?OSMRLS(name),
	?OSMRLS(type)
];

fs(grid) ->
	fs(basic);

fs(import) -> [
	itf:attachment()
];

fs(all) -> [
	itf:id(),
	itf:rev(),
	itf:attachment()
] ++
	fs(basic).

%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	layout(wf:q(mode)).


layout(?EDIT) ->
	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mod_rules_api:get(Id),
	Fs = fs({edit, Doc}),


	%
	% layout
	%
	Es = itl:get(?EDIT, itf:d2f(Doc, Fs), ite:get(edit), table),
	Sec = layout:grow(itl:section("Edit & Save", Es)),
	itl:page(heading(), layout:grow(layout:g(9, Sec)), links(Id));


layout(_) ->
	itxdocument:layout(wf:q(mode), ?MODULE, wf:q(id)).


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(edit) ->
	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mod_rules_api:get(Id),
	Fs = fs({edit, Doc}),

	Fsui = itf:uivalue(itf:d2f(Doc, Fs)),
	FsAll = itf:d2f(Doc, fs(all)),
	Res = itxdocuments:save(ep_osm_mod_rules_api:db(), Fsui, FsAll, Id),
	itl:flash(Res, "Saved", "Failed");


event(E) ->
	itxdocument:event(E, ?MODULE, wf:q(id)).



%------------------------------------------------------------------------------
% events - file
%------------------------------------------------------------------------------

start_upload_event(attachment_upload) ->
	itxdocument:start_upload_event(attachment_upload).

finish_upload_event(attachment_upload, AttachmentName, LocalFileData, Node) ->
	itxdocument:finish_upload_event(attachment_upload, AttachmentName, LocalFileData, Node).

%------------------------------------------------------------------------------
% links
%------------------------------------------------------------------------------
links() ->
	helper_ui:authorised_links(
		?MODULE,
		[?GRID, ?CREATE, ?SEARCH],
		itxauth:role(),
		undefined
	).

links(undefined) ->
	links();

links(Id) ->
	helper_ui:authorised_links(
		?MODULE,
		[?VIEW, ?EDIT],
		itxauth:role(),
		Id
	).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------
import(List) ->
	?D(List).

