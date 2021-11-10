-module(ep_osm_exam).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("ep_osm_exam").

heading() ->
	?LN("ep_osm_exam").

db() ->
	ep_osm_exam_api:db().

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, _) -> false.

%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

fs(basic) ->
	fields:getfields(anptest:fids());

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

fs(grid) ->
	fs(basic);

fs(import) ->
	Vals = [
		["faculty_code", "Faculty will be created if not found"],
		["faculty_name", ""],
		["program_code", "Program will be created if not found"],
		["program_name", ""],
		["program_pattern", ""],
		["subject_code", "Subject will be created if not found"],
		["subject_name", ""],
		["subject_pattern", ""],
		["testname", "Exam name"],
		["anptestcourseid", "Exam code"],
		["testtotalmarks", "Total marks"],
		["testduration", "Exam duration in minutes"],
		["pages_per_booklet", "Number of pages in the booklet used for this exam"],
		["startdate", "Date of exam"]
	],
	Es = dig:layout_vals(#dig{}, Vals, ["Field", "Description"]),

	[
		?ITXF({sep, Es}),
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

