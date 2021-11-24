-module(ep_osm_scanuploader).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("ep_osm_scanuploader").

heading() ->
	?LN("ep_osm_scanuploader").

db() ->
	ep_osm_scanuploader_api:db().

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, _) -> false.

%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

fs(basic) -> [
	itf:build(?ITXPRF(profiletype), ?APPOSM_SCANUPLOADER),
	?ITXPRF(username),
	?ITXPRF(fullname),
	?ITXPRF(mobile),
	?ITXPRF(email),
	?OSMCAP(osm_cap_fk),
	?ITXPRF(password_bcrypt)
];

fs(view) ->
	fs(basic);

fs(create) ->
	fs(basic);

fs(edit) ->
	fs(basic);

fs(update) ->
	fs(basic);

fs(search) -> [
	itf:build(?ITXPRF(profiletype), ?APPOSM_SCANUPLOADER),
	?ITXPRF(username),
	?ITXPRF(mobile),
	?ITXPRF(email)
];

fs(form) -> [
	?ITXPRF(username),
	?ITXPRF(fullname),
	?ITXPRF(mobile),
	?ITXPRF(email),
	?OSMCAP(osm_cap_fk)
];

fs(displayfs) -> [
	?ITXPRF(username),
	?ITXPRF(fullname),
	?ITXPRF(mobile),
	?ITXPRF(email)
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
	itxdocument:layout(wf:q(mode), ?MODULE, wf:q(id)).


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
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

