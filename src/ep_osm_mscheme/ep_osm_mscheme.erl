-module(ep_osm_mscheme).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Marking Scheme").

heading() ->
	?LN("OSM Marking Scheme").

db() ->
	ep_osm_mscheme_api:db().

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, _) -> false.

%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

fs(basic) -> [
	?OSMMSC(name),
	?OSMMSC(state),
	?OSMMSC(scheme)
];

fs(index) -> [
	?OSMMSC(name),
	?OSMMSC(state)
];

fs(view) ->
	fs(basic);

fs(create) -> [
	?OSMMSC(name)
];

fs(edit) ->
	fs(basic);

fs(update) ->
	fs(basic);

fs(search) ->
	fs(basic);

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
	Es = layout(wf:q(mode), wf:q(id)),
	layout:g(4, 4, Es).


%..............................................................................
%
% layout - create
%
%..............................................................................

layout(?CREATE, _) ->
	itl:get(?CREATE, fs(create), ite:get(create), table);



%..............................................................................
%
% layout - edit
%
%..............................................................................

layout(?EDIT, Id) when Id /= []; Id /= undefined ->


	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mscheme_api:get(Id),
	SchemeStr = itf:val(Doc, scheme),


	%
	% init marking rules
	%
	ListofRules = helper:l2t(SchemeStr),
	helper:state(anpcandidate_state_marking, ListofRules),


	%
	% layout
	%
	anp_marking:layout_marking_rules(anpmarking_anpevaluator, []);



%..............................................................................
%
% layout - other
%
%..............................................................................

layout(_, _) ->
	[].


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(create) ->
	handle_create();

event(E) ->
	itxdocument:event(E, ?MODULE, wf:q(id)).


%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - create
%
%..............................................................................

handle_create() ->

	%
	% init
	%
	FsUi = itf:uivalue(fs(create)),
	FsCreate = FsUi ++ [
		itf:build(?OSMMSC(state), "draft"),
		itf:build(?OSMMSC(scheme), helper:t2l([]))
	],


	%
	% save
	%
	case ep_osm_mscheme_api:save(FsCreate) of
		{ok, Doc} ->
			Url = io_lib:format("/~p?mode=edit&id=~s", [?MODULE, itf:idval(Doc)]),
			helper:redirect(Url);
		Error ->
			?D(Error),
			helper_ui:flash("Sorry, could not create!")
	end.


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


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
