-module(ep_osm_bundle).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("ep_osm_bundle").

heading() ->
	?LN("ep_osm_bundle").

db() ->
	"ep_osm_bundle".

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, _) -> false.

%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

fs(basic) -> [
	?COREXS(season_fk),
	?OSMBDL(osm_exam_fk),
	?OSMBDL(number),
	?OSMBDL(createdby),
	?OSMBDL(createdon),
	?OSMBDL(scannedby),
	?OSMBDL(qualityby),
	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate),
	?OSMBDL(bundle_size),
	?OSMBDL(inward_date),
	?OSMBDL(scanned_date),
	?OSMBDL(uploaded_date),
	?OSMBDL(comments)
];

fs(index) -> [
	?OSMBDL(osm_exam_fk),
	?OSMBDL(number),
	?OSMBDL(createdby),
	?OSMBDL(createdon),
	?OSMBDL(scannedby),
	?OSMBDL(qualityby),
	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate),
	?OSMBDL(inward_date),
	?OSMBDL(scanned_date),
	?OSMBDL(uploaded_date)
];

fs(form) -> [
	?OSMBDL(osm_exam_fk),
	?OSMBDL(number),
	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate),
	?OSMBDL(inward_date),
	?OSMBDL(scanned_date),
	?OSMBDL(uploaded_date)
];

fs(view) ->
	fs(basic);

fs(create) ->
	fs(basic);

fs(edit) -> [
	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate)
];

fs(update) ->
	fs(basic);

fs(search) ->
	fs(basic);

fs(grid) ->
	fs(basic);

fs(import) -> [
	?ITXF({sep, #pre {text="subject_code,bundle_number,anpseatnumber"}}),
	itf:hidden(?F(osm_exam_fk)),
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

