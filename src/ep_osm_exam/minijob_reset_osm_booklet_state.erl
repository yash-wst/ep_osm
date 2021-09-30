
-module(minijob_reset_osm_booklet_state).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(itx, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered.html"}).

title() ->
	?LN("minijob_reset_osm_booklet_state").

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
		dig_ep_osm_exam_evaluation_stats:f(reset_beyond_days),
		dig_ep_osm_exam_evaluation_stats:f(reset_booklet_state)
	] ++ dig_ep_osm_exam_evaluation_stats:fs(search).


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
	% init
	%
	ForgottenDays = ?S2I(itf:val(Doc, reset_beyond_days)),
	{FromState, ToState} = dig_ep_osm_exam_evaluation_stats:get_reset_from_to_states(
		itf:val(Doc, reset_booklet_state)
	),


	%
	% get docs
	%
	SearchFs = dig_ep_osm_exam_evaluation_stats:fs(search),
	SearchFs1 = itf:fs_delete(SearchFs, itf:hidden(osm_exam_fk)),
	SearchFs2 = itf:d2f(Doc, SearchFs1),
	SearchFs3 = dig:get_nonempty_fs(SearchFs2),
	Docs = ep_osm_exam_api:fetch(0, ?INFINITY, SearchFs3),


	%
	% do
	%
	dig_ep_osm_exam_evaluation_stats:handle_reset_forgotten_active_booklets(
		ForgottenDays, {FromState, ToState}, Docs
	).


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

