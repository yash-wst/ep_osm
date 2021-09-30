
-module(minijob_osm_result_export).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(itx, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered.html"}).

title() ->
	?LN("minijob_osm_result_export").

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
fs(Doc) ->

	%
	% get dig module
	%
	DigModule = ?L2A(itf:val(Doc, dig_module)),


	%
	% return fields
	%
	Dig = DigModule:get(),
	Dig#dig.filters ++ fs().


%
% fs -
%
fs() -> [
	itf:build(itf:textbox(?F(dig_module, "Dig Module")), "dig_ep_osm_exam_results")
].


%------------------------------------------------------------------------------
% concurrency
%------------------------------------------------------------------------------

concurrency() ->
	one_per_module.



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
	minijob:create_and_run(?MODULE, Fs ++ fs()).



%------------------------------------------------------------------------------
% do
%------------------------------------------------------------------------------

do(Doc) ->


	%
	% init
	%
	Uid = helper:uidintstr(),
	Dir = "/tmp/" ++ Uid,
	DigModule = ?L2A(itf:val(Doc, dig_module)),


	%
	% create dir
	%
	helper:cmd("mkdir -p ~s", [Dir]),


	%
	% export in batches
	%
	Fs = itf:d2f(Doc, DigModule:fs(search)),
	Fs1 = itf:fs_delete(Fs, itf:hidden(osm_exam_fk)),
	done = dig_ep_osm_exam_results:handle_export_results_bulk(Fs1, Dir, 0),


	%
	% zip and upload dir
	%
	Basename = filename:basename(Dir),
	helper:cmd("mv ~s /tmp", [Dir]),
	helper:cmd("cd /tmp; zip -r ~s.zip ~s", [
		Basename, Basename
	]),


	%
	% upload
	%
	Zipfilepath = ?FLATTEN(io_lib:format("/tmp/~s.zip", [Basename])),
	Zipfilename = itx:format("~s.zip", [Basename]),
	{ok, _} = minijob_api:upload_file(Doc, Zipfilename, Zipfilepath),



	%
	% clean
	%
	helper:cmd("rm -rf ~s", [Dir]).



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

