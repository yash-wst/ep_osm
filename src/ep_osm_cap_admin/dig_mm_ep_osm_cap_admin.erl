
-module(dig_mm_ep_osm_cap_admin).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("CAP Centre Admin").

heading() ->
	title().

form() ->
	ep_osm_cap_admin.


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fs
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fs - group
%------------------------------------------------------------------------------

fields(ep_osm_cap_admin, _Fs) ->
	ep_osm_cap_admin:fs(form).



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?ADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		mode=?VIEW,
		module=?MODULE,
		filters=ep_osm_cap_admin:fs(search),
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

%..............................................................................
%
% []
%
%..............................................................................
fetch(D, From, Size, Fs) ->
	dig_mm:fetch(D, From, Size, Fs).



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	dig_mm:layout(?MODULE).



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event(E) ->
	dig_mm:event(E).

start_upload_event(Event) ->
	dig_mm:start_upload_event(Event).

finish_upload_event(Tag, AttachmentName, LocalFileData, Node) ->
	dig_mm:finish_upload_event(Tag, AttachmentName, LocalFileData, Node).

%------------------------------------------------------------------------------
% assertions
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% save
%------------------------------------------------------------------------------


%
% override before save function
%
before_save(FsToSave, _FsAll, _Doc) ->
	FsToSave.


%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------