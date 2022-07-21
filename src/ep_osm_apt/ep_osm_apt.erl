-module(ep_osm_apt).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("OSM Appointment").

heading() ->
	title().

db() ->
	ep_osm_apt_api:db().

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?ADMIN) -> true;
access(_, _) -> false.

%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

fs(basic) -> [
	?COREXS(season_fk),
	?CORFAC(faculty_code_fk),
	?CORPGM(program_code_fk),
	?CORSUB(subject_code_fk),
	?OSMAPT(apt_number),
	?OSMAPT(apt_state),
	?OSMAPT(evaluator_id),
	?OSMAPT(evaluator_type),
	?OSMAPT(evaluator_state)
];

fs(template) -> [
	?COREXS(season_fk),
	?CORFAC(faculty_code_fk),
	?CORPGM(program_code_fk),
	?CORSUB(subject_code_fk),
	?OSMAPT(apt_number),
	?OSMAPT(apt_state),
	?OSMAPT(evaluator_id),
	?OSMAPT(evaluator_type),
	?OSMAPT(evaluator_state),
	?OSMAPT(evaluator_fullname),
	?OSMAPT(evaluator_mobile),
	?OSMAPT(evaluator_email)
];

fs(view) ->
	fs(basic);

fs(create) ->
	fs(basic);

fs(edit) ->
	fs(basic);

fs(update) ->
	fs(basic);

fs(search) ->
	fs(basic);

fs(index) ->
	fs(search);

fs(grid) ->
	fs(basic);

fs(import) ->
	[
		?ITXF({sep, #pre {text="username,fullname,mobile,email,ip,faculty_code,program_code,subject_code"}}),
		?COREXS(season_fk, #field {id=import_season_fk}),
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

layout("pdf") ->
	{ok, Doc} = ep_osm_apt_api:get(wf:q(id)),
	Es = itl:get(?VIEW, itf:d2f(Doc, fs(view)), ite:get(pdf, "PDF"), table),
	akit_fullpage:layout(Es, links(wf:q(id)));

layout(_) ->
	itxdocument:layout(wf:q(mode), ?MODULE, wf:q(id)).


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(pdf) ->
	handle_create_pdf(wf:q(id));

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
% handlers
%------------------------------------------------------------------------------

handle_create_pdf(Id) ->
	{ok, AppDoc} = ep_osm_apt_api:get(Id),
	{ok, TDoc} = ep_core_template_api:get_templatedoc_for_type("ep_osm_apt"),
	{Filename, Filepath} = ep_core_template_handler:handle_generate_pdf(TDoc, AppDoc),
	itxdownload:stream(Filename, Filepath).



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
		[?VIEW, ?EDIT, "pdf"],
		itxauth:role(),
		Id
	).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------
import(List) ->
	?D(List).

