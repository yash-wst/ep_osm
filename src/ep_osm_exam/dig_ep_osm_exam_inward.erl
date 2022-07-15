
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
access(_, ?APPOSM_QC) -> true;
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
		],
		size=100
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
	SeasonId = itf:val(ExamDoc, season_fk),
	{ok, SeasonDoc} = ep_core_exam_season_api:get(SeasonId),
	IsBundleActive = is_bundle_active(SeasonDoc, ExamDoc),



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
			#dcell {val=itf:val(CDoc, total_pages)},
			#dcell {val=layout_uploaded_pages(ExamDoc, BundleDoc, CDoc)},
			#dcell {val=layout_candidate_edit(BundleDoc, CDoc)},
			#dcell {val=layout_candidate_remove(BundleDoc, CDoc)}
		]
	end, CandidateDocs1),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Barcode / UID"},
		#dcell {type=header, val="Seat No."},
		#dcell {type=header, val="State"},
		#dcell {type=header, val="Student Name"},
		#dcell {type=header, val="Total Pages"},
		#dcell {type=header, val="Uploaded Images"},
		#dcell {type=header, val="Edit"},
		#dcell {type=header, val="Remove"}
	],


	%
	% return
	%
	{D#dig {
		total=length(CandidateDocs1),
		config=D#dig.config ++ [
			{show_slno, true}
		],
		description=#link {
			url=itx:format("/dig_ep_osm_exam_inward?id=~s", [OsmExamId]),
			text=io_lib:format("~s / ~s / ~s / Bundle: ~s (~s)", [
				itf:val(ExamDoc, anptestcourseid),
				itf:val(ExamDoc, testname),
				?LN(?L2A(itf:val(ExamDoc, teststatus))),
				itf:val(BundleDoc, number),
				get_bundle_state(BundleDoc)
			])
		},
		actions=dig_ep_osm_exam_inward_actions:layout_actions_bundle(
			BundleDoc, IsBundleActive
		)
	}, [Header] ++ Results};



%..............................................................................
%
% [osm_exam_fk]
%
%..............................................................................
fetch(D, From, Size, [
	#field {id=osm_exam_fk, uivalue=OsmExamId} | _
] = Fs) ->

	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(OsmExamId),
	SeasonId = itf:val(ExamDoc, season_fk),
	{ok, SeasonDoc} = ep_core_exam_season_api:get(SeasonId),	
	IsBundleActive = is_bundle_active(SeasonDoc, ExamDoc),


	%
	% get bundle docs
	%
	#db2_find_response {docs=BundleDocs} = db2_find:get_by_fs(
		ep_osm_bundle_api:db(), Fs, From, Size
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
			itf:val(BDoc, qualityby),
			itf:val(BDoc, qcby)
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
		QCState = itf:val(BDoc, qcstate),
		BundleStates = {InwardState, ScanningState, UploadState, QCState},

		[
			#dcell {val=itf:val(BDoc, number)},
			#dcell {
				bgcolor=get_bgcolor(inwardstate, BundleStates),
				val=[
					itx:format("~s ~s", [InwardState, itf:val(BDoc, inward_date)]),
					layout_user_info(dict:find(itf:val(BDoc, createdby), ProfileDocsDict))
				]
			},
			#dcell {
				bgcolor=get_bgcolor(scanningstate, BundleStates),
				val=[
					itx:format("~s ~s", [ScanningState, itf:val(BDoc, scanned_date)]),
					layout_dtp_by(scannedby, BDoc, ProfileDocsDict, BundleStates)
				]
			},
			#dcell {
				bgcolor=get_bgcolor(uploadstate, BundleStates),
				val=[
					itx:format("~s ~s", [UploadState, itf:val(BDoc, uploaded_date)]),
					layout_dtp_by(qualityby, BDoc, ProfileDocsDict, BundleStates)
				]
			}
		] ++ case ep_osm_config:is_qc_enabled() of
			true -> [
				#dcell {
					bgcolor=get_bgcolor(qcstate, BundleStates),
					val=[
						itx:format("~s ~s", [itf:val(BDoc, qcstate), itf:val(BDoc, qc_date)]),
						layout_dtp_by(qcby, BDoc, ProfileDocsDict, BundleStates)
					]
				}
			];
			false -> [
			]
		end ++ [
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
	% header
	%
	Header = [
		#dcell {type=header, val="Bundle Number"},
		#dcell {type=header, val="Inward"},
		#dcell {type=header, val="Scan"},
		#dcell {type=header, val="Upload"}
	] ++ case ep_osm_config:is_qc_enabled() of
		true -> [
			#dcell {type=header, val="QC"}
		];
		false -> [
		]
	end ++ [
		#dcell {type=header, val="Select"}
	],


	%
	% return
	%
	{D#dig {
		total=ep_osm_bundle_api:get_count_by_osm_exam_fk(OsmExamId),
		description=io_lib:format("~s / ~s", [
			itf:val(ExamDoc, testname), ?LN(?L2A(itf:val(ExamDoc, teststatus)))
		]),
		actions=dig_ep_osm_exam_inward_actions:layout_actions_exam(
			IsBundleActive
		)
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
% layout - uploaded pages
%
%..............................................................................

layout_uploaded_pages(ExamDoc, BundleDoc, CDoc) ->
	layout_uploaded_pages(ExamDoc, BundleDoc, CDoc, itf:val(BundleDoc, scanningstate)).


layout_uploaded_pages(ExamDoc, _BundleDoc, CDoc, ?COMPLETED) ->
	Bucket = helper_s3:aws_s3_bucket(),
	DirPath = itx:format("~s/~s/", [
		itf:val(ExamDoc, aws_s3_dir), itf:val(CDoc, anpseatnumber)
	]),
	try

		%
		% init
		%
		TotalPages = helper:l2i(itf:val(CDoc, total_pages)),
		ExpectedImagesCount = get_expected_images_from_total_pages(TotalPages),
		Keys = helper_s3:list_keys_config(Bucket, DirPath, [
			{extension, ".jpg"}
		]),
		LengthKeys = length(Keys),


		%
		% class
		%
		HighlightClass = case ExpectedImagesCount == LengthKeys of
			true ->
				"p-1";
			_ ->
				"text-danger border border-danger border-1 p-1"
		end,


		%
		% layout
		%
		#link {
			new=true,
			class=HighlightClass,
			text=LengthKeys,
			url=itx:format("/ep_osm_verify_images?anpid=~s&anptestid=~s", [
				itf:idval(CDoc), itf:idval(ExamDoc)
			])
		}
	catch _:_ ->
		error
	end;
layout_uploaded_pages(_ExamDoc, _BundleDoc, _CDoc, _) ->
	[].

%..............................................................................
%
% layout - candidate remove
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
% layout - candidate edit
%
%..............................................................................
layout_candidate_edit(BundleDoc, CDoc)  ->

	%
	% init
	%
	User = itxauth:user(),

	case {
		itf:val(BundleDoc, createdby),
		itf:val(BundleDoc, inwardstate),
		itf:val(BundleDoc, scanningstate)
	} of
		{_, "discarded", _} ->
			[];
		{User, InwardState, ScanningState} when
			InwardState == [];
			ScanningState == [] ->
			ite:button(
				edit_candidate,
				"Edit",
				{edit_candidate, itf:idval(CDoc)},
				"btn btn-link"
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
	ExamId = wf:q(id),
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),
	PagesPerBooklet = itf:val(ExamDoc, pages_per_booklet),
	Fs = [
		itf:textbox(?F(anp_paper_uid, "Barcode / UID"), [], textbox_enterkey),
		itf:textbox(?F(anpseatnumber, "Student Seat No."), [], textbox_enterkey),
		itf:build(
			itf:textbox(?F(total_pages, "Total Pages"), [], textbox_enterkey),
			PagesPerBooklet
		)
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
			helper_s3:aws_s3_bucket(),
			configs:get(aws_s3_access_key, []),
			configs:get(aws_s3_secret, []),
			configs:get(aws_s3_default_region, []),
			ObjectKey,
			RedirectUrl
		])
	],

	%
	% previous uploads
	%
	EsPreviousUploads = case cfg_show_previous_uploads() of
		true ->
			minijob_ep_osm_exam_uploadtos3:layout_previous_uploads(BundleDoc);
		_ ->
			[]
	end,	


	[
		itl:instructions([
			{danger, "Folder name and zip file name should be same. Ex: Folder: 200, Zip: 200.zip"},
			{ok, "Zip file should contain folders of the current bundle only."},
			{danger, "Please ensure you have reliable, high bandwidth, internet connection"}
		]),
		EsPreviousUploads,
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
layout_dtp_by(_Type, _BundleDoc, _ProfileDocsDict, {"discarded", _, _}) ->
	[];

layout_dtp_by(Type, BundleDoc, ProfileDocsDict, BundleStates) ->
	layout_dtp_by(
		itxauth:role(),
		Type,
		BundleDoc,
		ProfileDocsDict,
		itf:val(BundleDoc, Type),
		BundleStates
	).


%
% receiver
%
layout_dtp_by(?APPOSM_RECEIVER, scannedby = Type, BundleDoc, _ProfileDocsDict, [], 
	{InwardState, _, _, _}) when
		InwardState == ?COMPLETED ->
	ite:button(
		launch_assign_dtp_staff, "Assign", {launch_assign_dtp_staff, Type, BundleDoc}
	);


%
% scanner uploader
%
layout_dtp_by(?APPOSM_SCANUPLOADER, scannedby, BundleDoc, ProfileDocsDict, User,
	{_, ScanningState, _, _}) when
		ScanningState /= ?COMPLETED, User /= [] ->
	[
		layout_user_info(dict:find(User, ProfileDocsDict)),
		ite:button(
			scanning_completed, "Mark Scanning Completed", {scanning_completed, BundleDoc}
		)
	];

layout_dtp_by(?APPOSM_SCANUPLOADER, scannedby = Type, BundleDoc, _ProfileDocsDict, [],
	{InwardState, ScanningState, _, _}) when 
		InwardState == ?COMPLETED, ScanningState /= ?COMPLETED ->
	ite:button(
		assign_bundle, "Assign", {assign_bundle, Type, BundleDoc}, "btn btn-info"
	);

layout_dtp_by(?APPOSM_SCANUPLOADER, qualityby = Type, BundleDoc, _ProfileDocsDict, [],
	{_, ScanningState, UploadState, _}) when 
		ScanningState == ?COMPLETED, UploadState /= ?COMPLETED ->
	ite:button(
		assign_bundle, "Assign", {assign_bundle, Type, BundleDoc}, "btn btn-info"
	);



%
% qc
%
layout_dtp_by(?APPOSM_QC, qcby = Type, BundleDoc, _ProfileDocsDict, [],
	{_, _, UploadState, QCState}) when 
		UploadState == ?COMPLETED, QCState /= ?COMPLETED ->
	ite:button(
		assign_bundle, "Assign", {assign_bundle, Type, BundleDoc}, "btn btn-info"
	);



%
% default
%
layout_dtp_by(_Role, _Type, _BundleDoc, ProfileDocsDict, Val, _BundleStates) ->
	case Val of
		[] ->
			[];
		_ ->
			layout_user_info(dict:find(Val, ProfileDocsDict))
	end.






%------------------------------------------------------------------------------
% events - file
%------------------------------------------------------------------------------

start_upload_event(_) ->
	helper_ui:flash(warning, "Uploading. Please wait ...").

finish_upload_event({_,file_import_student_data} = Tag, AttachmentName, LocalFileData, _Node) ->
	dig_ep_osm_exam:finish_upload_event(Tag, AttachmentName, LocalFileData, _Node);

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
	dig_ep_osm_exam_inward_uploadtos3:upload(
		S3Dir, DirNamesToUpload, Filename, Fileloc, wf:q(osm_bundle_fk)
	).



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


	case cfg_show_previous_uploads() of
		true ->
			redirect_to_bundle(OsmBundleId);
		_ ->
			minijob_status:show_status(Doc)
	end.



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event({confirmation_yes, discard_bundle}) ->
	dig_ep_osm_exam_inward_handler:handle_discard_bundle();
	
event(discard_bundle = E) ->
	itl:confirmation(
		[
			"Are you sure you want to discard the entire bundle?",
			itl:instructions(instructions_discard_bundle())
		],
		E
	);

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
	dig_ep_osm_exam_inward_handler:handle_export_bundle_dir(wf:q(osm_exam_fk), wf:q(osm_bundle_fk));

event(import_master_data) ->
	dig_mm:handle_show_action("Import Student Data", dig_ep_osm_exam:layout_import_student_data());


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
	dig_ep_osm_exam_inward_handler:handle_remove_candidate(CId);

event({remove_candidate, _CId} = E) ->
	itl:confirmation(
		"Are you sure you want to delete this entry?",
		E
	);

event({edit_candidate, CId}) ->
	dig_ep_osm_exam_inward_handler:handle_edit_candidate_dialog(CId);

event({edit_receiver, CId}) ->
	dig_ep_osm_exam_inward_handler:handle_edit_candidate(CId);

event({confirmation_yes, inward_completed}) ->
	dig_ep_osm_exam_inward_handler:handle_inward_completed(wf:q(osm_exam_fk), wf:q(osm_bundle_fk));

event(inward_completed) ->
	itl:confirmation(
		"Are you sure you want to mark this bundle as 'Inward Completed'?",
		inward_completed
	);


event({confirmation_yes, upload_completed}) ->
	itl:modal_close(),
	dig_ep_osm_exam_inward_handler:handle_upload_completed(wf:q(osm_exam_fk), wf:q(osm_bundle_fk));


event(upload_completed) ->
	itl:confirmation(
		"Are you sure you want to mark this bundle as 'Upload Completed'?",
		upload_completed
	);


event({confirmation_yes, scanning_completed}) ->
	dig_ep_osm_exam_inward_handler:handle_scanning_completed();


event({confirmation_yes, {scanning_completed, BundleDoc}}) ->
	dig_ep_osm_exam_inward_handler:handle_scanning_completed(itf:idval(BundleDoc));

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

event({confirmation_yes, qc_completed}) ->
	dig_ep_osm_exam_inward_handler:handle_qc_completed(wf:q(osm_exam_fk), wf:q(osm_bundle_fk));

event(qc_completed) ->
	itl:confirmation(
		"Are you sure you want to mark this bundle as 'QC Completed'?",
		qc_completed
	);

event({confirmation_yes, {Type, BundleDoc}}) ->
	dig_ep_osm_exam_inward_handler:handle_assign_bundle(Type, BundleDoc);


event({confirmation_yes, {assign_dtp_staff, Type, BundleDoc, User}}) ->
	dig_ep_osm_exam_inward_handler:handle_assign_bundle(Type, BundleDoc, User);

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
		qualityby -> "Uploading";
		qcby -> "QC"
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
	dig_ep_osm_exam_inward_handler:handle_export_bundle_csv(
		wf:q(osm_exam_fk), wf:q(osm_bundle_fk)
	);


event(print_bundle_cover) ->
	dig_ep_osm_exam_inward_handler:handle_print_bundle_cover(
		wf:q(osm_exam_fk), wf:q(osm_bundle_fk)
	);


event(textbox_enterkey) ->
	dig_ep_osm_exam_inward_handler:handle_inward(
		wf:q(anp_paper_uid), wf:q(anpseatnumber), wf:q(total_pages)
	);


event(refresh) ->
	dig:refresh();


event(create_bundle) ->
	dig_ep_osm_exam_inward_handler:handle_create_bundle(
		wf:q(osm_exam_fk),
		wf:q(packet_number),
		wf:q(rack_location)
	);

event(create_bundle_form) ->
	dig_ep_osm_exam_inward_handler:handle_create_bundle_form();

event({itx, E}) ->
	ite:event(E).



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



get_bgcolor(inwardstate, {"completed", _, _, _}) ->
	"table-success";
get_bgcolor(scanningstate, {"completed", [], _, _}) ->
	"table-danger";
get_bgcolor(scanningstate, {"completed", "assigned", _, _}) ->
	"table-warning";
get_bgcolor(scanningstate, {"completed", "completed", _, _}) ->
	"table-success";
get_bgcolor(uploadstate, {"completed", "completed", [], _}) ->
	"table-danger";
get_bgcolor(uploadstate, {"completed", "completed", "assigned", _}) ->
	"table-warning";
get_bgcolor(uploadstate, {"completed", "completed", "completed", _}) ->
	"table-success";
get_bgcolor(qcstate, {"completed", "completed", "completed", []}) ->
	"table-danger";
get_bgcolor(qcstate, {"completed", "completed", "completed", "assigned"}) ->
	"table-warning";
get_bgcolor(qcstate, {"completed", "completed", "completed", "completed"}) ->
	"table-success";
get_bgcolor(_, {_, _, _, _}) ->
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


redirect_to_bundle(OsmBundleId) ->
	Url = itx:format("/~p?id=~s&digx=~s", [
		?MODULE, wf:q(id), base64:encode_to_string(helper:t2l([
			{osm_exam_fk, wf:q(id)},
			{osm_bundle_fk, OsmBundleId}
		]))
	]),
	helper:redirect(Url).




get_expected_images_from_total_pages(error) ->
	error;
get_expected_images_from_total_pages(TotalPages) ->
	(TotalPages /2) + 1.



get_bundle_state(BundleDoc) ->
	case {
		itf:val(BundleDoc, uploadstate),
		itf:val(BundleDoc, scanningstate),
		itf:val(BundleDoc, inwardstate)
	} of
		{_, _, "discarded"} ->
			"Discarded";
		{?COMPLETED, _, _} ->
			"Upload Completed";
		{_, ?COMPLETED, _} ->
			"Scanning Completed";
		{_, _, ?COMPLETED} ->
			"Inward Completed";
		{Is, Ss, Us} ->
			itx:format("~s_~s_~s", [Is, Ss, Us])
	end.




is_bundle_active(SeasonDoc, ExamDoc) ->
	case {itf:val(SeasonDoc, state), itf:val(ExamDoc, teststatus)} of
		{?ACTIVE, TestStatus} when TestStatus /= ?COMPLETED ->
			true;
		_ ->
			false
	end.


%------------------------------------------------------------------------------
% asserts
%------------------------------------------------------------------------------


%..............................................................................
%
% assert - entry does not exist elsewhere
%
%..............................................................................

assert_entry_does_not_exist_elsewhere(UId, SNo, CandidateDoc) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(wf:q(osm_exam_fk)),


	%
	% seat number
	%
	case itf:val(CandidateDoc, anpseatnumber) of
		SNo ->
			skip;
		_ ->
			FsToSearchCandidate = [
				itf:build(itf:textbox(?F(anpseatnumber)), SNo)
			],
			#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
				ExamDb, FsToSearchCandidate, 0, ?INFINITY, [
					{use_index, ["anpseatnumber"]}
				]
			),
			case CandidateDocs of
				[] ->
					ok;
				[CandidateDocFound] ->
					BundleIdFound = itf:val(CandidateDocFound, osm_bundle_fk),
					BundleNumberFound = get_bundle_number_from_cache(BundleIdFound),
					?ASSERT(
						false,
						itx:format("Seat number ~s: already exists in bundle ~s", [
							SNo, BundleNumberFound
						])
					)
			end
	end,


	%
	% uid
	%
	case itf:val(CandidateDoc, anp_paper_uid) of
		[] ->
			skip;
		UId ->
			skip;
		_ ->
			FsToSearchCandidate1 = [
				itf:build(itf:textbox(?F(anp_paper_uid)), UId)
			],
			#db2_find_response {docs=CandidateDocs1} = db2_find:get_by_fs(
				ExamDb, FsToSearchCandidate1, 0, ?INFINITY, [
					{use_index, ["anp_paper_uid"]}
				]
			),
			case CandidateDocs1 of
				[] ->
					ok;
				[CandidateDocFound1] ->
					BundleIdFound1 = itf:val(CandidateDocFound1, osm_bundle_fk),
					BundleNumberFound1 = get_bundle_number_from_cache(BundleIdFound1),
					?ASSERT(
						false,
						itx:format("UId ~s: already exists in bundle ~s", [
							UId, BundleNumberFound1
						])
					)
			end
	end.




%------------------------------------------------------------------------------
% configs
%------------------------------------------------------------------------------

cfg_show_previous_uploads() ->
	itxconfigs_cache:get2(dig_ep_osm_exam_inward_show_previous_uploads, true).



%------------------------------------------------------------------------------
% instructions
%------------------------------------------------------------------------------

instructions_discard_bundle() ->
	[
		{ok, "This operation applies only to seat numbers in 'Not Uploaded' state"},
		{ok, "All applicable seat numbers will be moved to 'Discarded' state"},
		{ok, "Seat number will change from SNO to SNO_discarded"},
		{ok, "Bundle inward state will change to Discarded"}
	].



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
