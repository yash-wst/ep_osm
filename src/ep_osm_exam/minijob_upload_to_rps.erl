
-module(minijob_upload_to_rps).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(itx, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("minijob_upload_to_rps").

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
	dig_ep_osm_exam_upload_to_result_processing_system:fs(search) ++
	dig_ep_osm_exam_upload_to_result_processing_system:fs(minijob).


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
	dig_ep_osm_exam_upload_to_result_processing_system:handle_upload_actual(
		itf:val(Doc, season_fk),
		itf:val(Doc, frp_season_fk),
		itf:val(Doc, osm_profiletype),
		itf:val(Doc, frp_mark_type),
		itf:d2f(Doc, dig_ep_osm_exam_upload_to_result_processing_system:fs(search))
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

