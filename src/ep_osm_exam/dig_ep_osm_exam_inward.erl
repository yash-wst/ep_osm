
-module(dig_ep_osm_exam_inward).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("OSM Inward").

heading() ->
	title().

form() ->
	ep_osm_bundle.

%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(osm_exam_fk = I) ->
	F = itf:textbox(?F(I, "OSM Exam")),
	F#field {
		renderer=fun(_Mode, _Event, #field {label=L, uivalue=Id}) ->
				{L, [
					#hidden {id=I, text=Id},
					layout_osm_exam_name(Id)
				]}
		end
	}.



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, ?APPOSM_CONTROLLER) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, ?APPOSM_SCANUPLOADER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	%
	% init
	%
	OsmExamId = wf:q(id),


	#dig {
		module=?MODULE,
		filters=[
			itf:build(f(osm_exam_fk), OsmExamId),
			?OSMBDL({osm_bundle_fk, OsmExamId}),
			?OSMBDL(inwardstate),
			?OSMBDL(scanningstate),
			?OSMBDL(uploadstate)
		],
		config=[
			{action_layout_type, buttons}
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Inward").



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
% [osm_exam_fk, osm_bundle_fk]
%
%..............................................................................

fetch(D, _From, _Size, [
	#field {id=osm_exam_fk, uivalue=OsmExamId},
	#field {id=osm_bundle_fk, uivalue=OsmBundleId}
]) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(OsmExamId),
	{ok, ExamDoc} = ep_osm_exam_api:get(OsmExamId),
	{ok, BundleDoc} = ep_osm_bundle_api:get(OsmBundleId),



	%
	% get student docs from osm exam db with the specified bundle id
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY
	),
	CandidateDocs1 = sort_candidate_docs(CandidateDocs),



	%
	% results
	%
	Results = lists:map(fun(CDoc) ->
		[
			#dcell {val=itf:val(CDoc, anp_paper_uid)},
			#dcell {val=itf:val(CDoc, anpseatnumber)},
			#dcell {val=?LN(?L2A(itf:val(CDoc, anpstate)))},
			#dcell {val=itf:val(CDoc, anpfullname)},
			#dcell {val=layout_candidate_remove(BundleDoc, CDoc)}
		]
	end, CandidateDocs1),


	%
	% actions
	%
	Actions = [
		{print_bundle_cover, "Print Bundle Cover", "Print Bundle Cover"},
		{export_bundle_csv, "Export Bundle CSV", "Export Bundle CSV"},
		{export_bundle_dir, "Export Bundle Folder", "Export Bundle Folder"}
	] ++
		layout_action_inwarding(BundleDoc) ++
		layout_action_scanning(BundleDoc) ++
		layout_action_uploading(BundleDoc) ++
		layout_action_inward_form(BundleDoc),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Barcode / UID"},
		#dcell {type=header, val="Seat No."},
		#dcell {type=header, val="State"},
		#dcell {type=header, val="Student Name"},
		#dcell {type=header, val="Remove"}
	],


	%
	% return
	%
	{D#dig {
		description=#link {
			url=itx:format("/dig_ep_osm_exam_inward?id=~s", [OsmExamId]),
			text=io_lib:format("~s / ~s / ~s / Bundle: ~s", [
				itf:val(ExamDoc, anptestcourseid),
				itf:val(ExamDoc, testname),
				?LN(?L2A(itf:val(ExamDoc, teststatus))),
				itf:val(BundleDoc, number)
			])
		},
		actions=Actions
	}, [Header] ++ Results};



%..............................................................................
%
% [osm_exam_fk]
%
%..............................................................................
fetch(D, _From, _Size, [
	#field {id=osm_exam_fk, uivalue=OsmExamId} | _
] = Fs) ->

	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(OsmExamId),
	#db2_find_response {docs=BundleDocs} = db2_find:get_by_fs(
		ep_osm_bundle_api:db(), Fs, 0, ?INFINITY
	),
	FBundle = ?OSMBDL({osm_bundle_fk, OsmExamId}),


	%
	% sort bundles by number
	%
	BundleDocsSorted = lists:sort(fun(A, B) ->
		itf:val(A, createdon) < itf:val(B, createdon)
	end, BundleDocs),


	%
	% get profile ids
	%
	Usernames = lists:foldl(fun(BDoc, Acc) ->
		Acc ++ [
			itf:val(BDoc, createdby),
			itf:val(BDoc, scannedby),
			itf:val(BDoc, qualityby)
		]
	end, [], BundleDocsSorted),
	UsernamesUnique = helper:unique(Usernames),


	%
	% get profile dict
	%
	ProfileDocs = itxprofiles:getdocs_by_usernames(UsernamesUnique),
	ProfileDocsDict = helper:get_dict_from_docs(ProfileDocs, username),



	%
	% results
	%
	Results = lists:map(fun(BDoc) ->

		InwardState = itf:val(BDoc, inwardstate),
		ScanningState = itf:val(BDoc, scanningstate),
		UploadState = itf:val(BDoc, uploadstate),

		[
			#dcell {val=itf:val(BDoc, number)},
			#dcell {
				bgcolor=get_bgcolor(inwardstate, InwardState, ScanningState, UploadState),
				val=[
					itx:format("~s ~s", [
						itf:val(BDoc, inwardstate),
						itf:val(BDoc, inward_date)
					]),
					layout_user_info(dict:find(itf:val(BDoc, createdby), ProfileDocsDict))
				]
			},
			#dcell {
				bgcolor=get_bgcolor(scanningstate, InwardState, ScanningState, UploadState),
				val=[
					itx:format("~s ~s", [
						itf:val(BDoc, scanningstate),
						itf:val(BDoc, scanned_date)
					]),
					layout_dtp_by(
						scannedby, BDoc, ProfileDocsDict, {InwardState, ScanningState, UploadState}
					)
				]
			},
			#dcell {
				bgcolor=get_bgcolor(uploadstate, InwardState, ScanningState, UploadState),
				val=[
					itx:format("~s ~s", [
						itf:val(BDoc, uploadstate),
						itf:val(BDoc, uploaded_date)
					]),
					layout_dtp_by(
						qualityby, BDoc, ProfileDocsDict, {InwardState, ScanningState, UploadState}
					)
				]
			},
			#dcell {
				val=#span {
					class="btn btn-sm btn-primary-outline",
					body="Bundle #" ++ itf:val(BDoc, number)
				},
				postback={filter, itf:build(FBundle, itf:idval(BDoc))}
			}
		]
	end, BundleDocsSorted),


	%
	% actions
	%
	Actions = case itxauth:role() of
		?APPOSM_RECEIVER -> [
			{create_bundle, "Create New Bundle", "Create New Bundle"},
			{action_import, "+ Import", "+ Import"}
		];
		_ -> [
		]
	end,


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Bundle Number"},
		#dcell {type=header, val="Inward"},
		#dcell {type=header, val="Scan"},
		#dcell {type=header, val="QC / Uploader"},
		#dcell {type=header, val="Select"}
	],


	%
	% return
	%
	{D#dig {
		description=io_lib:format("~s / ~s", [
			itf:val(ExamDoc, testname), ?LN(?L2A(itf:val(ExamDoc, teststatus)))
		]),
		actions=Actions
	}, [Header] ++ Results};


%..............................................................................
%
% []
%
%..............................................................................
fetch(D, _From, _Size, _Fs) ->
	{D, []}.


%------------------------------------------------------------------------------
% function - exports
%------------------------------------------------------------------------------
exports() -> [
].



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	[
		dig:dig(?MODULE:get())
	].



%..............................................................................
%
% layout - inward form
%
%..............................................................................
layout_candidate_remove(BundleDoc, CDoc)  ->

	%
	% init
	%
	User = itxauth:user(),

	case {itf:val(BundleDoc, createdby), itf:val(BundleDoc, inwardstate)} of
		{User, []} ->
			ite:button(
				remove_candidate,
				"x",
				{remove_candidate, itf:idval(CDoc)},
				"btn btn-sm btn-danger-outline btn-outline-danger"
			);
		_ ->
			[]
	end.



%..............................................................................
%
% layout - inward form
%
%..............................................................................

layout_inward_form() ->
	Fs = [
		itf:textbox(?F(anp_paper_uid, "Barcode / UID"), [], textbox_enterkey),
		itf:textbox(?F(anpseatnumber, "Student Seat No."), [], textbox_enterkey)
	],
	itl:get(?CREATE, Fs, noevent, line).



%..............................................................................
%
% layout - upload form
%
%..............................................................................

layout_upload_form(BundleDoc, _) ->

	%
	% init
	%
	ObjectKey = ?FLATTEN(io_lib:format("~s_~s.zip", [
		itf:idval(BundleDoc),
		helper:uidintstr()
	])),

	RedirectUrl = ?FLATTEN(io_lib:format("https://~s/~p?id=~s&digx=~s&objectkey=~s", [
		wf:header(host),
		?MODULE,
		wf:q(id),
		wf:q(digx),
		ObjectKey
	])),

	Es = [
		itxfile_s3_upload:form([
			configs:get(aws_s3_bucket, []),
			configs:get(aws_s3_access_key, []),
			configs:get(aws_s3_secret, []),
			configs:get(aws_s3_default_region, []),
			ObjectKey,
			RedirectUrl
		])
	],
	[
		itl:instructions([
			{danger, "Folder name and zip file name should be same. Ex: Folder: 200, Zip: 200.zip"},
			{ok, "Zip file should contain folders of the current bundle only."},
			{danger, "Please ensure you have reliable, high bandwidth, internet connection"}
		]),
		Es
	].



%..............................................................................
%
% layout - user info
%
%..............................................................................

layout_user_info({ok, ProfileDoc}) ->
	itl:blockquote([
		itf:val(ProfileDoc, fullname)
	]);
layout_user_info(_) ->
	"ERROR!".




%..............................................................................
%
% layout - dtp by
%
%..............................................................................

layout_dtp_by(Type, BundleDoc, ProfileDocsDict, BundleStates) ->
	layout_dtp_by(
		itxauth:role(),
		Type,
		BundleDoc,
		ProfileDocsDict,
		itf:val(BundleDoc, Type),
		BundleStates
	).

layout_dtp_by(
	?APPOSM_RECEIVER,
	scannedby = Type,
	BundleDoc,
	_ProfileDocsDict,
	[],
	{InwardState, _, _}) when
		InwardState == ?COMPLETED ->
	ite:link(
		launch_assign_dtp_staff, "Assign", {launch_assign_dtp_staff, Type, BundleDoc}
	);

layout_dtp_by(
	?APPOSM_SCANUPLOADER,
	scannedby,
	BundleDoc,
	ProfileDocsDict,
	User,
	{_, ScanningState, _}) when
		ScanningState /= ?COMPLETED, User /= [] ->
	[
		layout_user_info(dict:find(User, ProfileDocsDict)),
		ite:link(
			scanning_completed, "Scanning completed", {scanning_completed, BundleDoc}
		)
	];

layout_dtp_by(
	Role,
	Type,
	BundleDoc,
	_ProfileDocsDict,
	[],
	_BundleStates)
	when Role == ?APPOSM_SCANUPLOADER ->
	ite:button(
		assign_bundle, "Assign", {assign_bundle, Type, BundleDoc}, "btn btn-info"
	);

layout_dtp_by(
	_Role,
	_Type,
	_BundleDoc,
	ProfileDocsDict,
	Val,
	_BundleStates) ->
	case Val of
		[] ->
			[];
		_ ->
			layout_user_info(dict:find(Val, ProfileDocsDict))
	end.




%..............................................................................
%
% layout - action inward form
%
%..............................................................................

layout_action_inward_form(BundleDoc) ->

	%
	% init
	%
	User = itxauth:user(),
	handle_uploaded_zip_file(),

	%
	% action
	%
	case {itf:val(BundleDoc, createdby), itf:val(BundleDoc, inwardstate)} of
		{User, []} ->
			event(inward_form),
			[
				{inward_form, "Inward Form", "Inward Form"}
			];
		_ -> [
		]
	end.


%..............................................................................
%
% layout - action inwarding
%
%..............................................................................

layout_action_inwarding(BundleDoc) ->

	%
	% init
	%
	User = itxauth:user(),


	%
	% action
	%
	case {itf:val(BundleDoc, createdby), itf:val(BundleDoc, inwardstate)} of
		{User, []} -> [
			{inward_completed, "Inward Completed", "Inward Completed"}
		];
		_ -> [
		]
	end.


%..............................................................................
%
% layout - action scanning
%
%..............................................................................

layout_action_scanning(BundleDoc) ->

	%
	% init
	%
	User = itxauth:user(),


	%
	% action
	%
	case {itf:val(BundleDoc, scannedby), itf:val(BundleDoc, scanningstate)} of
		{User, "assigned"} -> [
			{scanning_completed, "Scanning Completed", "Scanning Completed"}
		];
		_ -> [
		]
	end.



%..............................................................................
%
% layout - action uploading
%
%..............................................................................

layout_action_uploading(BundleDoc) ->

	%
	% init
	%
	User = itxauth:user(),


	%
	% action
	%
	case {itf:val(BundleDoc, qualityby), itf:val(BundleDoc, uploadstate)} of
		{User, "assigned"} ->
			event({upload_form, BundleDoc}),
			[
				{upload_form, "Upload Form", "Upload Form"},
				{upload_completed, "Uploading Completed", "Uploading Completed"}
			];
		_ -> [
		]
	end.



%------------------------------------------------------------------------------
% events - file
%------------------------------------------------------------------------------

start_upload_event(_) ->
	helper_ui:flash(warning, "Uploading. Please wait ...").


finish_upload_event(Tag, Filename, Fileloc, Node) ->
	case string:to_lower(filename:extension(Filename)) of
		".csv" ->
			dig_mm:finish_upload_event(Tag, Filename, Fileloc, Node);
		_ ->
			finish_upload_event_inward(Tag, Filename, Fileloc, Node)
	end.

finish_upload_event_inward(_Id, Filename, Fileloc, _Node) ->

	%
	% init
	%
	helper_ui:flash_clear(),
	dig:log("File received. File is being processed. Please wait ..."),


	%
	% get docs
	%
	ExamId = wf:q(osm_exam_fk),
	{ok, ExamDoc} = anptests:getdoc(ExamId),
	BundleDocs = get_bundle_docs(),


	%
	% upload
	%
	S3Dir = itf:val(ExamDoc, aws_s3_dir),
	DirNamesToUpload = lists:map(fun(D) ->
		itf:val(D, anpseatnumber)
	end, BundleDocs),
	dig_ep_osm_exam_inward_uploadtos3:upload(S3Dir, DirNamesToUpload, Filename, Fileloc).



finish_upload_event_inward_minijob(ObjectKey) ->

	%
	% init
	%
	ExamId = wf:q(osm_exam_fk),
	OsmBundleId = wf:q(osm_bundle_fk),
	{ok, ExamDoc} = anptests:getdoc(ExamId),
	S3Dir = itf:val(ExamDoc, aws_s3_dir),


	{ok, Doc} = minijob_ep_osm_exam_uploadtos3:create_and_run([
		itf:build(?OSMEXM(osm_exam_fk), ExamId),
		itf:build(?OSMBDL(osm_bundle_fk), OsmBundleId),
		itf:build(minijob_ep_osm_exam_uploadtos3:f(objectkey), ObjectKey),
		itf:build(minijob_ep_osm_exam_uploadtos3:f(aws_s3_dir), S3Dir)
	]),


	minijob_status:show_status(Doc).



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(upload_form) ->
	{ok, BundleDoc} = ep_osm_bundle_api:get(wf:q(osm_bundle_fk)),
	event({upload_form, BundleDoc});

event({upload_form, BundleDoc}) ->
	dig_mm:handle_show_action(
		"Upload: (zip bundle directory and upload)",
		layout_upload_form(BundleDoc, undefined)
	);


event(inward_form) ->
	dig_mm:handle_show_action(
		"Inward Form: (enter barcode or seat number and hit enter)",
		layout_inward_form()
	);

event(export_bundle_dir) ->
	handle_export_bundle_dir(wf:q(osm_exam_fk), wf:q(osm_bundle_fk));

event({browser_to_s3_completed, _BundleDoc, ObjectKey}) ->
	case configs:getbool(process_via_minijob, false) of
		false ->
			finish_upload_event_inward(undefined, ObjectKey, undefined, undefined);
		true ->
			finish_upload_event_inward_minijob(ObjectKey)
	end;



event(action_import) ->
	dig_mm:event(action_import);


event({confirmation_yes, {remove_candidate, CId}}) ->
	handle_remove_candidate(CId);

event({remove_candidate, _CId} = E) ->
	itl:confirmation(
		"Are you sure you want to delete this entry?",
		E
	);


event({confirmation_yes, inward_completed}) ->
	handle_inward_completed();

event(inward_completed) ->
	itl:confirmation(
		"Are you sure you want to mark this bundle as 'Inward Completed'?",
		inward_completed
	);


event({confirmation_yes, upload_completed}) ->
	itl:modal_close(),
	handle_upload_completed(wf:q(osm_exam_fk), wf:q(osm_bundle_fk));


event(upload_completed) ->
	itl:confirmation(
		"Are you sure you want to mark this bundle as 'Upload Completed'?",
		upload_completed
	);


event({confirmation_yes, scanning_completed}) ->
	handle_scanning_completed();


event({confirmation_yes, {scanning_completed, BundleDoc}}) ->
	handle_scanning_completed(itf:idval(BundleDoc));

event({scanning_completed, BundleDoc}) ->
	itl:confirmation(
		itx:format("Are you sure you want to mark this bundle ~s as 'Scanning Completed'?", [
			itf:val(BundleDoc, number)
		]),
		{scanning_completed, BundleDoc}
	);

event(scanning_completed) ->
	itl:confirmation(
		"Are you sure you want to mark this bundle as 'Scanning Completed'?",
		scanning_completed
	);


event({confirmation_yes, {Type, BundleDoc}}) ->
	handle_assign_bundle(Type, BundleDoc);


event({confirmation_yes, {assign_dtp_staff, Type, BundleDoc, User}}) ->
	handle_assign_bundle(Type, BundleDoc, User);

event({itx, {textbox_picker, {pick, ep_osm_scanuploader_fk, Val, _Label}}}) ->
	{Type, BundleDoc} = helper:state(launch_assign_dtp_staff),
	{ok, ProfileDoc} = ep_osm_scanuploader_api:get(Val),
	itl:confirmation(
		itx:format("Are you sure you want to assign ~s to bundle ~s", [
			itf:val(ProfileDoc, fullname), itf:val(BundleDoc, number)
		]),
		{assign_dtp_staff, Type, BundleDoc, itf:val(ProfileDoc, username)}
	);


event({launch_assign_dtp_staff, Type, BundleDoc}) ->
	helper:state(launch_assign_dtp_staff, {Type, BundleDoc}),
	textbox_picker:event({launch, ?OSMSUP(ep_osm_scanuploader_fk)});


event({assign_bundle, Type, BundleDoc}) ->

	TypeLabel = case Type of
		scannedby -> "Scanning";
		qualityby -> "Uploading"
	end,

	itl:confirmation(
		#panel {class="mycenter", body=[
			#p {
				class="lead font-weight-bold",
				text=TypeLabel
			},
			#p {
				text="Are you sure you want to assign this bundle to yourself?"
			}
		]},
		{Type, BundleDoc}
	);


event(export_bundle_csv) ->
	handle_export_bundle_csv(wf:q(osm_exam_fk), wf:q(osm_bundle_fk));


event(print_bundle_cover) ->
	handle_print_bundle_cover(wf:q(osm_exam_fk), wf:q(osm_bundle_fk));


event(textbox_enterkey) ->
	handle_inward(wf:q(anp_paper_uid), wf:q(anpseatnumber));


event(refresh) ->
	dig:refresh();


event({confirmation_yes, create_bundle}) ->
	handle_create_bundle(wf:q(osm_exam_fk));


event(create_bundle) ->
	itl:confirmation(
		#panel {class="mycenter", body=[
			#p {text="Are you sure you want to create a new bundle?"},
			#p {text="Please create new bundle only after previous bundle is full"}
		]},
		create_bundle
	);


event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------

%..............................................................................
%
% handle - export bundle folder
%
%..............................................................................

handle_export_bundle_dir(ExamId, BundleId) ->
	%
	% init
	%
	ExamDb = anpcandidates:db(ExamId),
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),
	{ok, BundleDoc} = ep_osm_bundle_api:get(BundleId),
	BundleDirName = get_bundle_dir_name(ExamDoc, BundleDoc),


	%
	% prepare cover uids
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY
	),
	CandidateDocs1 = sort_candidate_docs(CandidateDocs),
	{_, CandidateDirs} = lists:foldl(fun(CandidateDoc, {AccIndex, Acc}) ->
		{
			AccIndex + 1,
			Acc ++ [
				string:join([?I2S(AccIndex), itf:val(CandidateDoc, anpseatnumber)], ".")
			]
		}
	end, {1, []}, CandidateDocs1),


	%
	% create directories
	%
	UId = helper:uidintstr(),
	WorkDir = ?FLATTEN(io_lib:format("/tmp/~s/~s", [UId, BundleDirName])),
	helper:cmd("mkdir -p ~s; cd ~s; mkdir ~s", [
		WorkDir, WorkDir, string:join(CandidateDirs, " ")
	]),


	%
	% zip dir
	%
	helper:cmd("cd /tmp/~s; zip -r ~s.zip ~s", [UId, BundleDirName, BundleDirName]),


	%
	% clean up
	%
	helper:cmd("mv /tmp/~s/~s.zip /tmp/~s.zip", [UId, BundleDirName, UId]),
	helper:cmd("rm -rf /tmp/~s", [UId]),


	%
	% export
	%
	Filename = ?FLATTEN(io_lib:format("~s.zip", [BundleDirName])),
	Filepath = ?FLATTEN(io_lib:format("/tmp/~s.zip", [UId])),
	itxdownload:stream_and_delete_file(Filename, Filepath).



%..............................................................................
%
% handle - remove canddidate
%
%..............................................................................

handle_remove_candidate(CId) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(wf:q(osm_exam_fk)),


	%
	% delete
	%
	case anpcandidates:delete(ExamDb, CId) of
		{ok, _} ->
			dig:refresh();
		_ ->
			helper_ui:flash(error, "Sorry, could not delete!")
	end.


%..............................................................................
%
% handle - inward completed
%
%..............................................................................

handle_inward_completed() ->

	%
	% init
	%
	Id = wf:q(osm_bundle_fk),


	%
	% fs to save
	%
	FsToSave = [
		itf:build(?OSMBDL(inwardstate), "completed"),
		itf:build(?OSMBDL(inward_date), helper:date_today_str()),
		itf:build(?OSMBDL(bundle_size), ?I2S(length(get_bundle_docs())))
	],


	%
	% save
	%
	case ep_osm_bundle_api:save(FsToSave, ep_osm_bundle:fs(all), Id) of
		{ok, _} ->
			redirect_to_main();
		_ ->
			helper_ui:flash(error, "Sorry, could not save!")
	end.




%..............................................................................
%
% handle - upload completed
%
%..............................................................................

handle_upload_completed(ExamId, BundleId) ->


	%
	% init
	%
	ExamDb = anpcandidates:db(ExamId),
	{ok, ExamDoc} = anptests:getdoc(ExamId),
	S3Dir = itf:val(ExamDoc, aws_s3_dir),



	%
	% get all candidate docs of this bundle
	%
	FsToSearch = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded")
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearch, 0, ?INFINITY
	),


	%
	% check their upload status on s3
	%
	lists:map(fun(CDoc) ->

		%
		%  get keys
		%
		UploadedDir = ?FLATTEN(S3Dir ++ "/" ++ itf:val(CDoc, anpseatnumber)),
		Keys = helper_s3:get_keys(UploadedDir, ".jpg"),


		%
		% asset - ensure dir is not empty
		%
		?ASSERT(
			length(Keys) > 0,
			io_lib:format("ERROR: ~s is empty!", [UploadedDir])
		),


		%
		% log files in dir
		%
		dig:log(io_lib:format("~s: total files: ~p", [UploadedDir, length(Keys)]))

	end, CandidateDocs),


	%
	% change candidate state to uploaded
	%
	ListOfFsDoc = lists:map(fun(CDoc) ->
		FsDoc = helper_api:doc2fields({ok, CDoc}),
		itf:fs_merge(FsDoc, [
			fields:build(anpstate, "anpstate_yettostart")
		])
	end, CandidateDocs),
	{ok, _} = anpcandidates:savebulk(ExamDb, ListOfFsDoc),



	%
	% update bundle state to completed
	%
	handle_uploading_completed().


%..............................................................................
%
% handle - uploading completed
%
%..............................................................................

handle_uploading_completed() ->

	%
	% init
	%
	Id = wf:q(osm_bundle_fk),


	%
	% fs to save
	%
	FsToSave = [
		itf:build(?OSMBDL(uploadstate), "completed"),
		itf:build(?OSMBDL(uploaded_date), helper:date_today_str())
	],


	%
	% save
	%
	case ep_osm_bundle_api:save(FsToSave, ep_osm_bundle:fs(all), Id) of
		{ok, _} ->
			redirect_to_main();
		_ ->
			helper_ui:flash(error, "Sorry, could not save!")
	end.



%..............................................................................
%
% handle - scanning completed
%
%..............................................................................

handle_scanning_completed() ->
	Id = wf:q(osm_bundle_fk),
	handle_scanning_completed(Id).

handle_scanning_completed(Id) ->

	%
	% init
	%
	{ok, BundleDoc} = ep_osm_bundle_api:get(Id),


	%
	% fs to save
	%
	FsToSave = [
		itf:build(?OSMBDL(scanningstate), "completed"),
		itf:build(?OSMBDL(scanned_date), helper:date_today_str()),
		itf:build_comment(itf:d2f(BundleDoc, ?OSMBDL(comments)), "scanning completed")
	],


	%
	% save
	%
	case ep_osm_bundle_api:save(FsToSave, ep_osm_bundle:fs(all), Id) of
		{ok, _} ->
			redirect_to_main();
		_ ->
			helper_ui:flash(error, "Sorry, could not save!")
	end.




%..............................................................................
%
% handle - assign bundle
%
%..............................................................................

handle_assign_bundle(Type, BundleDoc) ->
	handle_assign_bundle(Type, BundleDoc, itxauth:user()).

handle_assign_bundle(Type, BundleDoc, User) ->

	%
	% init
	%
	StateFId = case Type of
		scannedby ->
			scanningstate;
		qualityby ->
			uploadstate
	end,
	FsToSave = [
		itf:build(?OSMBDL(Type), User),
		itf:build(?OSMBDL(StateFId), "assigned")
	],


	%
	% save and sure doc not already assigned
	%
	Id = itf:idval(BundleDoc),
	{ok, Doc} = ep_osm_bundle_api:get(Id),


	%
	% assert - not assigned
	%
	?ASSERT(
		itf:val(Doc, Type) == [],
		"ERROR: This bundle is already assigned!"
	),



	%
	% assert - cannot assign uplaod till bundle scanning is completed
	%
	case Type of
		scannedby ->
			?ASSERT(
				itf:val(Doc, inwardstate) == "completed",
				"ERROR: Cannot assign till inwarding has been completed!"
			);
		qualityby ->
			?ASSERT(
				itf:val(Doc, scanningstate) == "completed",
				"ERROR: Cannot assign till scanning has been completed!"
			);
		_ ->
			ok
	end,


	%
	% save
	%
	FsAll = ep_osm_bundle:fs(all),
	FsAll1 = itf:d2f(Doc, FsAll),
	FsAll2 = itf:fs_merge(FsAll1, FsToSave),
	case ep_osm_bundle_api:save(FsAll2) of
		{ok, _} ->
			dig:refresh();
		_ ->
			helper_ui:flash(error, "Sorry, could not assign!")
	end.



%..............................................................................
%
% handle - export bundle csv
%
%..............................................................................

handle_export_bundle_csv(ExamId, BundleId) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(ExamId),
	{ok, ExamDoc} = anptests:getdoc(ExamId),
	{ok, BundleDoc} = ep_osm_bundle_api:get(BundleId),
	{ok, SubjectDoc} = case itf:val(ExamDoc, subject_code_fk) of
		[] ->
			{ok, {[]}};
		_ ->
			ep_core_subject_api:get(itf:val(ExamDoc, subject_code_fk))
	end,
	SeasonName = ep_core_exam_season_api:getname(itf:val(ExamDoc, season_fk)),
	SubjectCode = itf:val(SubjectDoc, subject_code),
	BundleNumber = itf:val(BundleDoc, number),


	%
	% prepare cover uids
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY
	),


	%
	% layout dig cells
	%
	CandidateDocs1 = sort_candidate_docs(CandidateDocs),
	Results = lists:map(fun(CDoc) ->
		[
			#dcell {val=SubjectCode},
			#dcell {val=BundleNumber},
			#dcell {val=itf:val(CDoc, anpseatnumber)},
			#dcell {val=itf:val(CDoc, anpfullname)}
		]
	end, CandidateDocs1),


	%
	% export
	%
	{Name, FilePath} = dig:get_filename_path(
		io_lib:format("~s_~s_bundle_~s", [SeasonName, SubjectCode, BundleNumber])
	),
	dig:write_data_to_file(FilePath, Results),
	itxdownload:stream(Name, FilePath).





%..............................................................................
%
% handle - print bundle cover
%
%..............................................................................

handle_print_bundle_cover(ExamId, BundleId) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(ExamId),
	{ok, ExamDoc} = anptests:getdoc(ExamId),
	{ok, BundleDoc} = ep_osm_bundle_api:get(BundleId),
	{ok, ReceiverDoc} = itxprofiles:get_by(username, itf:val(BundleDoc, createdby), false),
	{ok, CapDoc} = ep_osm_cap_api:get(itf:val(ReceiverDoc, osm_cap_fk)),
	SeasonName = ep_core_exam_season_api:getname(itf:val(ExamDoc, season_fk)),


	%
	% prepare cover header
	%
	Es1 = #panel {
		class="font-weight-bold mycenter text-uppercase",
		body=[
			#p {
				style="margin: 0px;",
				text=SeasonName
			},
			#p {
				style="font-size: 1.5em; margin: 0px;",
				text=io_lib:format("(~s) ~s", [itf:val(CapDoc, code), itf:val(CapDoc, name)])
			},
			#p {
				style="font-size: 1.5em; margin: 0px;",
				text=ep_core_subject_api:getname(itf:val(ExamDoc, subject_code_fk))
			},
			#p {
				style="font-size: 1.5em; margin: 0px;",
				text=itf:val(ExamDoc, testname)
			},
			#p {
				style="font-size: 2em; margin: 0px;",
				text=itf:val(ExamDoc, anptestcourseid)
			},
			#p {
				style="font-size: 2em; margin: 0px;",
				text="# " ++ itf:val(BundleDoc, number)
			},
			#hr {}
		]
	},


	%
	% prepare cover uids
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY
	),
	CandidateDocs1 = sort_candidate_docs(CandidateDocs),
	ListOfCandidateDocs = helper:list_split(CandidateDocs1, 5),
	Results = lists:map(fun(CDocs) ->
		lists:map(fun(CDoc) ->
			UId = itf:val(CDoc, anp_paper_uid),
			SNo = itf:val(CDoc, anpseatnumber),
			#dcell {val=?CASE_IF_THEN_ELSE(SNo, [], UId, SNo)}
		end, CDocs)
	end, ListOfCandidateDocs),
	Table = dig:layout_table(#dig{show_slno=false}, Results),
	Es2 = Table#table {class="table table-sm table-bordered"},





	%
	% combine es
	%
	Es = [
		Es1,
		Es2
	],

	%
	% create pdf
	%
	{ok, Filepath} = helper_topdf:create_from_elements(
		"/tmp/", BundleId, [], Es, "portrait", "5", "5", "5", "5"
	),



	%
	% download
	%
	Filename = BundleId ++ ".pdf",
	itxdownload:stream_and_delete_file(Filename, Filepath).




%..............................................................................
%
% handle - inward
%
%..............................................................................

handle_inward([], []) ->
	helper_ui:flash(error, "Please enter either barcode or student seat number", 5);

handle_inward(UId, SNo) ->

	%
	% init
	%
	ExamId = wf:q(id),
	OsmBundleId = wf:q(osm_bundle_fk),
	ExamDb = anpcandidates:db(ExamId),



	%
	% assert - bundle is not full
	%
	BundleDocs = get_bundle_docs(),
	?ASSERT(
		length(BundleDocs) < 61,
		"bundle full; create new bundle"
	),



	%
	% search for existing docs
	%
	FsToSearchCandidate = [
		itf:build(itf:textbox(?F(anp_paper_uid)), UId),
		itf:build(itf:textbox(?F(anpseatnumber)), SNo)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchCandidate, 0, ?INFINITY
	),
	handle_inward(ExamId, OsmBundleId, UId, SNo, CandidateDocs).



handle_inward(ExamId, OsmBundleId, UId, SNo, []) ->
	%
	% create entry in exam db
	%

	ExamDb = anpcandidates:db(ExamId),
	BundleDoc = get_bundle_doc_from_cache(OsmBundleId),
	BundleNumber = get_bundle_number_from_cache(OsmBundleId),



	FsToSave = [
		itf:build(itf:textbox(?F(anp_paper_uid)), UId),
		itf:build(itf:textbox(?F(anpseatnumber)), ?CASE_IF_THEN_ELSE(SNo, [], UId, SNo)),
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId),
		itf:build(itf:textbox(?F(anpcentercode)), "B:" ++ BundleNumber),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded"),
		itf:build(itf:textbox(?F(timestamp_inward)), helper:epochtimestr())
	],
	case anpcandidates:save(ExamDb, FsToSave) of
		{ok, CandidateDoc} ->
			handle_inward_focus_textbox(),
			handle_insert_candidatedoc(BundleDoc, CandidateDoc),
			helper_ui:flash(success, io_lib:format("Created: ~s, ~s", [UId, SNo]), 5);
		_ ->
			helper_ui:flash(error, io_lib:format("Error!: ~s, ~s", [UId, SNo]))
	end;


handle_inward(ExamId, OsmBundleId, UId, SNo, [Doc]) ->

	%
	% init
	%
	BundleId = itf:val(Doc, osm_bundle_fk),
	BundleDoc = get_bundle_doc_from_cache(OsmBundleId),
	BundleNumber = get_bundle_number_from_cache(BundleId),

	?ASSERT(
		BundleId == [],
		io_lib:format("~s, ~s: already exists in bundle ~s", [
			UId, SNo, BundleNumber
		])
	),


	%
	% fs to save
	%
	FsToSave = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId),
		itf:build(itf:textbox(?F(timestamp_inward)), helper:epochtimestr())
	],
	case ep_osm_candidate_api:update(ExamId, Doc, FsToSave) of
		{ok, CandidateDoc} ->
			handle_inward_focus_textbox(),
			handle_insert_candidatedoc(BundleDoc, CandidateDoc),
			helper_ui:flash(success, io_lib:format("Updated: ~s, ~s, ~s", [
				UId, SNo, itf:val(Doc, anpfullname)
			]), 5);
		_ ->
			helper_ui:flash(error, io_lib:format("Error!: ~s, ~s", [UId, SNo]))
	end.



handle_inward_focus_textbox() ->
	wf:wire("
		obj('anp_paper_uid').value = '';
		obj('anpseatnumber').value = '';
		obj('anp_paper_uid').focus();
		obj('anp_paper_uid').select();
	").



handle_insert_candidatedoc(BundleDoc, CDoc) ->
	Row = #tablerow {cells=[
		#tablecell {body=itf:val(CDoc, anp_paper_uid)},
		#tablecell {body=itf:val(CDoc, anpseatnumber)},
		#tablecell {body=?LN(?L2A(itf:val(CDoc, anpstate)))},
		#tablecell {body=itf:val(CDoc, anpfullname)},
		#tablecell {body=layout_candidate_remove(BundleDoc, CDoc)}
	]},
	wf:insert_top(dig:id(table), Row).



%..............................................................................
%
% handle - create bundle
%
%..............................................................................

handle_create_bundle(ExamId) ->

	%
	% init
	%
	{ok, ExamDoc} = anptests:getdoc(ExamId),
	SeasonId = itf:val(ExamDoc, season_fk),


	%
	% fields to save
	%
	FsToSave = [
		itf:build(?COREXS(season_fk), SeasonId),
		itf:build(?OSMBDL(osm_exam_fk), ExamId),
		itf:build(?OSMBDL(createdby), itxauth:user()),
		itf:build(?OSMBDL(createdon), helper:epochtimestr())
	],


	%
	% save
	%
	case ep_osm_bundle_api:create(FsToSave) of
		{ok, _BundleDoc} ->
			dig:refresh();
		_ ->
			helper_ui:flash(error, "Sorry, could not create bundle!")
	end.



%..............................................................................
%
% handle - uploaded zip file
%
%..............................................................................

handle_uploaded_zip_file() ->
	handle_uploaded_zip_file(wf:q(objectkey)).


handle_uploaded_zip_file(undefined) ->
	ok;
handle_uploaded_zip_file(ObjectKey) ->
	%
	% before processing check if object exists
	%
	try
		_Infos = helper_s3:info_dir(
			configs:get(aws_s3_bucket, []),
			"browser_to_s3/" ++ ObjectKey
		),

		wf:wire(#event{
			type=timer,
			delay=100,
			postback={browser_to_s3_completed, undefined, ObjectKey}
		})

	catch
		E:M ->
			?D({E, M})
	end.






%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



get_bundle_dir_name(ExamDoc, BundleDoc) ->
	%
	% init
	%
	SeasonId = itf:val(ExamDoc, season_fk),
	SubjectId = itf:val(ExamDoc, subject_code_fk),
	{ok, SeasonDoc} = case SeasonId of
		[] ->
			{ok, {[]}};
		_ ->
			ep_core_exam_season_api:get(SeasonId)
	end,
	{ok, SubjectDoc} = case SubjectId of
		[] ->
			{ok, {[]}};
		_ ->
			ep_core_subject_api:get(SubjectId)
	end,
	AnpTestCourseId = itf:val(ExamDoc, anptestcourseid),


	%
	% build name
	%
	Dirname = io_lib:format("~s_~s_~s_~s", [
		itf:val(SeasonDoc, name),
		itf:val(SubjectDoc, subject_code),
		AnpTestCourseId,
		itf:val(BundleDoc, number)
	]),
	helper:sanitisestr(Dirname).



layout_osm_exam_name(undefined) ->
	[];
layout_osm_exam_name(OsmExamId) ->
	%
	% init
	%
	{ok, ExamDoc} = anptests:getdoc(OsmExamId),
	itl:blockquote([
		ep_core_exam_season_api:getname(itf:val(ExamDoc, season_fk)),
		itf:val(ExamDoc, anptestcourseid),
		itf:val(ExamDoc, testname),
		?LN(?L2A(itf:val(ExamDoc, teststatus))),
		itf:val(ExamDoc, exam_pattern)
	]).



get_bundle_docs() ->
	ExamId = wf:q(id),
	OsmBundleId = wf:q(osm_bundle_fk),
	get_bundle_docs(ExamId, OsmBundleId).


get_bundle_docs(ExamId, OsmBundleId) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(ExamId),


	%
	% find docs
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId)
	],
	Db2FindRec = db2_find:getrecord_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY
	),
	#db2_find_response {docs=BundleDocs} = db2_find:find(Db2FindRec#db2_find {
		fields=[
			itf:textbox(?F(anp_paper_uid)),
			itf:textbox(?F(anpseatnumber))
		]
	}),


	%
	% return docs
	%
	sort_candidate_docs(BundleDocs).



sort_candidate_docs(Docs) ->
	lists:sort(fun(A, B) ->
		case {itf:val(A, timestamp_inward), itf:val(B, timestamp_inward)} of
			{Ai, Bi} when Ai == []; Bi == [] ->
				true;
			{Ai, Bi} ->
				?S2I(Ai) < ?S2I(Bi)
		end
	end, Docs).



get_bgcolor(inwardstate, "completed", _, _) ->
	"table-success";
get_bgcolor(scanningstate, "completed", [], _) ->
	"table-danger";
get_bgcolor(scanningstate, "completed", "assigned", _) ->
	"table-warning";
get_bgcolor(scanningstate, "completed", "completed", _) ->
	"table-success";
get_bgcolor(uploadstate, "completed", "completed", []) ->
	"table-danger";
get_bgcolor(uploadstate, "completed", "completed", "assigned") ->
	"table-warning";
get_bgcolor(uploadstate, "completed", "completed", "completed") ->
	"table-success";
get_bgcolor(_, _, _, _) ->
	[].


get_bundle_number_from_cache([]) ->
	[];
get_bundle_number_from_cache(BundleId) ->
	BundleDoc = get_bundle_doc_from_cache(BundleId),
	itf:val(BundleDoc, number).


get_bundle_doc_from_cache([]) ->
	[];
get_bundle_doc_from_cache(BundleId) ->
	Fun = fun() ->
		{ok, BundleDoc} = ep_osm_bundle_api:get(BundleId),
		BundleDoc
	end,
	itxdoc_cache:get({get_bundle_doc_from_cache, BundleId}, Fun).



redirect_to_main() ->
	Url = itx:format("/~p?id=~s", [
		?MODULE, wf:q(id)
	]),
	helper:redirect(Url).

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
