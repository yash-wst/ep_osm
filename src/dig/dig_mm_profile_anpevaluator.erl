
-module(dig_mm_profile_anpevaluator).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("Manage ANP Evaluator").

heading() ->
	title().

form() ->
	profile_module().

module(import) ->
	profile_anpevaluator_import.

profile_module() ->
	case minijobcontext:q(mode) of
		[] ->
			profile_anpevaluator;
		undefined ->
			profile_anpevaluator;
		Module ->
			?L2A(Module)
	end.

digmm_links(Doc) ->
	Module = profile_module(),
	Fs = itf:d2f(Doc, [itf:id()]),
	helper_ui:layout_slinks(Module, Fs).


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

fields(_ProfileModule, _Fs) -> [
	?ITXPRF(username, #field{renderer=fun renderer_username/3}),
	?ITXPRF(fullname),
	?ITXPRF(mobile),
	?ITXPRF(email),
	?ITXPRF(ip_address),
	?CORSUB(subjects, #field {renderer=fun renderer_subjects/3})
].



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	ProfileModule = profile_module(),
	#dig {
		mode=?VIEW,
		module=?MODULE,
		filters=ProfileModule:fs(search),
		size=25,
		events=[
			ite:button(export, "CSV", {itx, {dig, export}})
		],
		config=[
			{responsive_type, collapse}
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("Manage ANP Evaluator").



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
% renderers
%------------------------------------------------------------------------------

%
% renderer subjects
%
renderer_subjects(_, _, #field {label=L, uivalue=SubjectIds0}) ->

	%
	% get subject docs from cache
	%
	SubjectIds = case SubjectIds0 of
		undefined ->
			[];
		_ ->
			SubjectIds0
	end,
	Docs = ep_core_subject_api:get_docs_from_cache(SubjectIds),
	SubjectCodes = lists:map(fun(Doc) ->
		itf:val(Doc, subject_code)
	end, Docs),
	{L, string:join(SubjectCodes, ",")}.


%
% renderer username
%
renderer_username(_, _, #field {label=L, uivalue=Username}) ->
	{L, Username}.

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
