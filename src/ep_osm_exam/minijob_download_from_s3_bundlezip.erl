
-module(minijob_download_from_s3_bundlezip).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(itx, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("minijob_download_from_s3_bundlezip").

heading() ->
	title().


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?ADMIN) -> true;
access(_, _) -> false.





%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(bundleid) ->
	itf:textbox(?F(bundleid, "Bundle Id")).


%
% fs - doc
%
fs(_Doc) ->
	fs().



%
% fs -
%
fs() -> [
	f(bundleid)
].


%------------------------------------------------------------------------------
% concurrency
%------------------------------------------------------------------------------

concurrency() ->
	one_per_user.



%------------------------------------------------------------------------------
% size
%------------------------------------------------------------------------------

size(_Doc) ->
	100.



%------------------------------------------------------------------------------
% progress
%------------------------------------------------------------------------------
progress(Doc) ->
	progress(Doc, 1).
progress(Doc, Increment) ->
	minijob_api:update_progress(Doc, Increment).



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------

layout() ->
	minijob:layout(?MODULE).



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event(E) ->
	minijob:event(?MODULE, E).



%------------------------------------------------------------------------------
% create and run
%------------------------------------------------------------------------------

create_and_run(Fs) ->
	minijob:create_and_run(?MODULE, Fs).



%------------------------------------------------------------------------------
% do
%------------------------------------------------------------------------------

do(Doc) ->

	%
	% do
	%
	BundleId = itf:val(Doc, bundleid),
	Url = ep_osm_bundle:handle_createzip(BundleId),


	%
	% update minijob
	%
	minijob_api:update(itf:idval(Doc), [
		itf:build(?MJOB(mj_link), Url)
	]).


%------------------------------------------------------------------------------
% commit
%------------------------------------------------------------------------------

commit() ->
	ok.


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

