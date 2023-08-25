
-module(minijob_import_from_rps).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(itx, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("minijob_import_from_rps").

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

f(auto_create_bundles = I) ->
	itf:dropdown(?F(I, "Auto Create Bundles?"), itf:options([
		?F(yes, "Yes"),
		?F(no, "No")
	]));

f(date_of_test) ->
	itf:date(?F(date_of_test, "Date of Test")).


%
% fs - doc
%
fs(_Doc) ->
	fs().



%
% fs -
%
fs() ->
	[
		f(date_of_test),
		f(auto_create_bundles)
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
	% needs to reset profileid from context as rps download prn
	% function looks for admin profile id in itx_profiles
	% which does not exist as this is cross-app call.
	%
	Context = erlang:get(itxcontext),
	erlang:put(itxcontext, Context#itxcontext {
		profileid=undefined
	}),

	%
	% start processing
	%
	DateOfExam = itf:val(Doc, date_of_test),
	AutoCreateBundles = itf:val(Doc, auto_create_bundles),
	dig_mm_ep_osm_exam_from_frp:handle_import_from_frp(DateOfExam, AutoCreateBundles).



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

