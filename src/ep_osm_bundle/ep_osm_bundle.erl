-module(ep_osm_bundle).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("OSM Bundle").

heading() ->
	?LN("OSM Bundle").

db() ->
	"ep_osm_bundle".

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(Mode, ?APPOSM_ADMIN) when 
	Mode == ?VIEW;
	Mode == ?EDIT;
	Mode == "createzip" ->
	true;
access(_, _) -> false.

%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

fs(basic) -> [
	?COREXS(season_fk),
	?OSMBDL(osm_exam_fk),

	?OSMBDL(number),
	?OSMBDL(packet_number),
	?OSMBDL(packet_count),
	?OSMBDL(rack_location),

	?OSMBDL(receivedby), % physical/bundle inward stage
	?OSMBDL(receivedon),
	?OSMBDL(createdby), % booklet inward stage
	?OSMBDL(createdon),
	?OSMBDL(scannedby),
	?OSMBDL(qualityby),
	?OSMBDL(qcby),

	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate),
	?OSMBDL(qcstate),

	?OSMBDL(bundle_size),
	?OSMBDL(inward_date),
	?OSMBDL(scanned_date),
	?OSMBDL(uploaded_date),
	?OSMBDL(qc_date),

	?OSMBDL(comments)
];

fs(index) -> [
	?OSMBDL(osm_exam_fk),
	?OSMBDL(number),
	?OSMBDL(packet_number),
	?OSMBDL(receivedby),
	?OSMBDL(receivedon),
	?OSMBDL(createdby),
	?OSMBDL(createdon),
	?OSMBDL(scannedby),
	?OSMBDL(qualityby),
	?OSMBDL(qcby),
	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate),
	?OSMBDL(qcstate),
	?OSMBDL(inward_date),
	?OSMBDL(scanned_date),
	?OSMBDL(uploaded_date),
	?OSMBDL(qc_date)
];

fs(form) -> [
	?COREXS(season_fk),
	?OSMBDL(osm_exam_fk),
	?OSMBDL(number),
	?OSMBDL(packet_number),
	?OSMBDL(packet_count),
	?OSMBDL(rack_location),
	?OSMBDL(bundle_size),
	?OSMBDL(receivedby),
	?OSMBDL(receivedon),
	?OSMBDL(createdon),
	?OSMBDL(createdby),
	?OSMBDL(scannedby),
	?OSMBDL(qualityby),
	?OSMBDL(qcby),
	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate),
	?OSMBDL(qcstate),
	?OSMBDL(inward_date),
	?OSMBDL(scanned_date),
	?OSMBDL(uploaded_date),
	?OSMBDL(qc_date),
	?ITXF({actions, ?MODULE})
];

fs(mybundle) -> [
	?OSMBDL(osm_exam_fk),
	?OSMBDL(number),
	?OSMBDL(packet_number),
	?OSMBDL(packet_count),
	?OSMBDL(rack_location),
	?OSMBDL(receivedby),
	?OSMBDL(createdby),
	?OSMBDL(scannedby),
	?OSMBDL(qualityby),
	?OSMBDL(qcby),
	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate),
	?OSMBDL(qcstate)
];

fs(pendingbundles) -> [
	?OSMBDL(inward_date),
	?OSMBDL(osm_exam_fk),
	?OSMBDL(number),
	?OSMBDL(packet_number),
	?OSMBDL(rack_location),
	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate),
	?OSMBDL(qcstate)
];

fs(displayfs) -> [
	?OSMBDL(number),
	?OSMBDL(packet_number),
	?OSMBDL(packet_count),
	?OSMBDL(rack_location),
	?OSMBDL(inwardstate),
	?OSMBDL(scanningstate),
	?OSMBDL(uploadstate),
	?OSMBDL(qcstate)
];


fs(displayfs_picked) -> [
	?OSMBDL(number),
	?OSMBDL(packet_number),
	?OSMBDL(rack_location)
];



fs(view) ->
	fs(basic);

fs(create) ->
	fs(basic);

fs(edit) -> [
	?OSMBDL(packet_number),
	?OSMBDL(packet_count),
	?OSMBDL(rack_location)
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
	layout(wf:q(mode)).

layout("createzip") ->
	BundleId = wf:q(id),
	{ok, Doc} = ep_osm_bundle_api:get(BundleId),
	Event = ite:get(createzip, "Create Zip"),
	Es = itl:get(?VIEW, itf:d2f(Doc, fs(all)), Event, table),
	akit_fullpage:layout(Es, links(BundleId));

layout(_) ->
	itxdocument:layout(wf:q(mode), ?MODULE, wf:q(id)).


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(createzip) ->
	{ok, MDoc} = minijob_download_from_s3_bundlezip:create_and_run([
		itf:build(minijob_download_from_s3_bundlezip:f(bundleid), wf:q(id))
	]),
	minijob_status:show_status(MDoc);

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
		[?VIEW, ?EDIT, "createzip"],
		itxauth:role(),
		Id
	).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------
import(List) ->
	?D(List).


%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------

handle_createzip(Id) ->

	%
	% init
	%
	{ok, Doc} = ep_osm_bundle_api:get(Id),
	ExamId = itf:val(Doc, osm_exam_fk),
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),
	S3Dir = itf:val(ExamDoc, aws_s3_dir),



	%
	% get candidate seat numbers of this bundle
	%
	CDocs = dig_ep_osm_exam_inward:get_bundle_docs(ExamId, Id),
	SeatNumbers = lists:map(fun(CDoc) ->
		itf:val(CDoc, anpseatnumber)
	end, CDocs),



	%
	% copy candidate folders to a tmp dir
	%
	WordDir = "/tmp/download_from_s3",
	BundleDirName = dig_ep_osm_exam_inward:get_bundle_dir_name(ExamDoc, Doc),
	BundleDir = itx:format("~s/~s", [WordDir, BundleDirName]),
	helper:cmd("mkdir -p ~s", [BundleDir]),
	lists:foreach(fun(SeatNumber) ->
		Comment = itx:format("Downloading ... ~s", [SeatNumber]),
		minijob_api:add_comment(Comment),
		helper:cmd("AWS_ACCESS_KEY_ID=~s AWS_SECRET_ACCESS_KEY=~s AWS_DEFAULT_REGION=~s aws s3 sync --only-show-errors s3://~s/~s/~s ~s/~s --delete", [
			configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region),
			helper_s3:aws_s3_bucket(), S3Dir, SeatNumber,
			BundleDir, SeatNumber
		])
	end, SeatNumbers),



	%
	% zip and upload file to s3
	%
	helper:cmd("cd ~s; zip -r ~s.zip ~s", [
		WordDir, BundleDirName, BundleDirName
	]),
	helper:cmd("AWS_ACCESS_KEY_ID=~s AWS_SECRET_ACCESS_KEY=~s AWS_DEFAULT_REGION=~s aws s3 cp ~s.zip s3://~s/download_from_s3/~s.zip", [
		configs:get(aws_s3_access_key), configs:get(aws_s3_secret), configs:get(aws_s3_default_region),
		BundleDir, helper_s3:aws_s3_bucket(), BundleDirName
	]),



	%
	% cleanup
	%
	helper:cmd("rm -rf ~s", [BundleDir]),
	helper:cmd("rm -rf ~s/~s.zip", [WordDir, BundleDirName]),



	%
	% create download url
	%
	Key = itx:format("download_from_s3/~s.zip", [
		BundleDirName
	]),
	Url = lists:flatten(io_lib:format("https://~s.~s/~s", [
		helper_s3:aws_s3_bucket(),
		configs:get(aws_s3_region, "s3.ap-south-1.amazonaws.com"),
		Key
	])),
	Url.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
