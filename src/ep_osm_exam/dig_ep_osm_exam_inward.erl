
-module(dig_ep_osm_exam_inward).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

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



	%
	% results
	%
	Results = lists:map(fun(CDoc) ->
		[
			#dcell {val=itf:val(CDoc, anp_paper_uid)},
			#dcell {val=itf:val(CDoc, anpseatnumber)},
			#dcell {val=?LN(?L2A(itf:val(CDoc, anpstate)))},
			#dcell {val=layout_candidate_remove(BundleDoc, CDoc)}
		]
	end, CandidateDocs),


	%
	% actions
	%
	Actions = [
		{print_bundle_cover, "Print Bundle Cover", "Print Bundle Cover"},
		{refresh, "Refresh", "Refresh"},
		{export_bundle_csv, "Export Bundle CSV", "Export Bundle CSV"}
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
		#dcell {type=header, val="Remove"}
	],


	%
	% return
	%
	{D#dig {
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
			#dcell {val=itl:render(itf:d2f(BDoc, ?OSMBDL(createdon)))},
			#dcell {
				bgcolor=get_bgcolor(inwardstate, InwardState, ScanningState, UploadState),
				val=[
					itf:val(BDoc, inwardstate),
					layout_user_info(dict:find(itf:val(BDoc, createdby), ProfileDocsDict))
				]
			},
			#dcell {
				bgcolor=get_bgcolor(scanningstate, InwardState, ScanningState, UploadState),
				val=[
					itf:val(BDoc, scanningstate),
					layout_dtp_by(scannedby, BDoc, ProfileDocsDict)
				]
			},
			#dcell {
				bgcolor=get_bgcolor(uploadstate, InwardState, ScanningState, UploadState),
				val=[
					itf:val(BDoc, uploadstate),
					layout_dtp_by(qualityby, BDoc, ProfileDocsDict)
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
		#dcell {type=header, val="Created On"},
		#dcell {type=header, val="Inward"},
		#dcell {type=header, val="Scan"},
		#dcell {type=header, val="QC / Uploader"},
		#dcell {type=header, val="Select"}
	],


	%
	% return
	%
	{D#dig {
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
				"btn btn-sm btn-danger-outline"
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

layout_upload_form(BundleDoc, ObjectKey) when ObjectKey /= undefined, ObjectKey /= [] ->

	%
	% before processing check if object exists
	%
	try
		_Infos = helper_s3:info_dir(
			configs:get(aws_s3_bucket, []),
			"browser_to_s3/" ++ ObjectKey
		),

		wf:wire(#event{type=timer, delay=100, postback={browser_to_s3_completed, BundleDoc, ObjectKey}}),
		#span {text=[]}

	catch
		E:M ->
			?D({E, M}),
			layout_upload_form(BundleDoc, undefined)
	end;


layout_upload_form(BundleDoc, _) ->

	%
	% init
	%
	ObjectKey = ?FLATTEN(io_lib:format("~s_~s.zip", [
		itf:idval(BundleDoc),
		helper:uidintstr()
	])),

	RedirectUrl = ?FLATTEN(io_lib:format("~s/~p?id=~s&digx=~s&objectkey=~s", [
		customer:get(mainserver_url),
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
		itf:val(ProfileDoc, fullname),
		itf:val(ProfileDoc, mobile)
	]);
layout_user_info(_) ->
	"ERROR!".




%..............................................................................
%
% layout - dtp by
%
%..............................................................................

layout_dtp_by(Type, BundleDoc, ProfileDocsDict) ->
	layout_dtp_by(itxauth:role(), Type, BundleDoc, ProfileDocsDict, itf:val(BundleDoc, Type)).


layout_dtp_by(Role, Type, BundleDoc, _ProfileDocsDict, []) when Role == ?APPOSM_SCANUPLOADER ->
	ite:button(
		assign_bundle, "Assign", {assign_bundle, Type, BundleDoc}, "btn btn-info"
	);
layout_dtp_by(_Role, _Type, _BundleDoc, ProfileDocsDict, Val) ->
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

	%
	% action
	%
	case {itf:val(BundleDoc, createdby), itf:val(BundleDoc, inwardstate)} of
		{User, []} -> [
			{form, layout_inward_form(), "Inward Form: (enter barcode or seat number and hit enter)"}
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
		{User, "assigned"} -> [
			{form, layout_upload_form(BundleDoc, wf:q(objectkey)), "Upload: (zip bundle directory and upload)"},
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


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({browser_to_s3_completed, _BundleDoc, ObjectKey}) ->
	finish_upload_event_inward(undefined, ObjectKey, undefined, undefined);


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

event(scanning_completed) ->
	itl:confirmation(
		"Are you sure you want to mark this bundle as 'Scanning Completed'?",
		scanning_completed
	);


event({confirmation_yes, {Type, BundleDoc}}) ->
	handle_assign_bundle(Type, BundleDoc);


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
		itf:build(?OSMBDL(inwardstate), "completed")
	],


	%
	% save
	%
	case ep_osm_bundle_api:save(FsToSave, ep_osm_bundle:fs(all), Id) of
		{ok, _} ->
			dig:refresh();
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
		itf:build(?OSMBDL(uploadstate), "completed")
	],


	%
	% save
	%
	case ep_osm_bundle_api:save(FsToSave, ep_osm_bundle:fs(all), Id) of
		{ok, _} ->
			dig:refresh();
		_ ->
			helper_ui:flash(error, "Sorry, could not save!")
	end.



%..............................................................................
%
% handle - scanning completed
%
%..............................................................................

handle_scanning_completed() ->

	%
	% init
	%
	Id = wf:q(osm_bundle_fk),


	%
	% fs to save
	%
	FsToSave = [
		itf:build(?OSMBDL(scanningstate), "completed")
	],


	%
	% save
	%
	case ep_osm_bundle_api:save(FsToSave, ep_osm_bundle:fs(all), Id) of
		{ok, _} ->
			dig:refresh();
		_ ->
			helper_ui:flash(error, "Sorry, could not save!")
	end.




%..............................................................................
%
% handle - assign bundle
%
%..............................................................................

handle_assign_bundle(Type, BundleDoc) ->

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
		itf:build(?OSMBDL(Type), itxauth:user()),
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
	{ok, SubjectDoc} = ep_core_subject_api:get(itf:val(ExamDoc, subject_code_fk)),
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
	Results = lists:map(fun(CDoc) ->
		[
			#dcell {val=SubjectCode},
			#dcell {val=BundleNumber},
			#dcell {val=itf:val(CDoc, anpseatnumber)}
		]
	end, CandidateDocs),


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
	ListOfCandidateDocs = helper:list_split(CandidateDocs, 5),
	Results = lists:map(fun(CDocs) ->
		lists:map(fun(CDoc) ->
			UId = itf:val(CDoc, anp_paper_uid),
			SNo = itf:val(CDoc, anpseatnumber),
			#dcell {val=?CASE_IF_THEN_ELSE(UId, [], SNo, UId)}
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
	BundleNumber = OsmBundleId,
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
	% assert -  a document in the osm exam db with same uid or seat number does
	% not exist already
	%
	FsToSearchCandidate = [
		itf:build(itf:textbox(?F(anp_paper_uid)), UId),
		itf:build(itf:textbox(?F(anpseatnumber)), SNo)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchCandidate, 0, ?INFINITY
	),
	?ASSERT(
		CandidateDocs == [],
		io_lib:format("~s, ~s: already exists in the exam database", [
			UId, SNo
		])
	),


	%
	% create entry in exam db
	%

	FsToSave = [
		itf:build(itf:textbox(?F(anp_paper_uid)), UId),
		itf:build(itf:textbox(?F(anpseatnumber)), ?CASE_IF_THEN_ELSE(SNo, [], UId, SNo)),
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId),
		itf:build(itf:textbox(?F(anpcentercode)), BundleNumber),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded")
	],
	case anpcandidates:save(ExamDb, FsToSave) of
		{ok, _} ->
			wf:wire("
				obj('anp_paper_uid').value = '';
				obj('anpseatnumber').value = '';
				obj('anp_paper_uid').focus();
				obj('anp_paper_uid').select();
			"),
			helper_ui:flash(success, io_lib:format("saved: ~s, ~s", [UId, SNo]), 5);
		_ ->
			helper_ui:flash(error, io_lib:format("error: ~s, ~s", [UId, SNo]))
	end.



%..............................................................................
%
% handle - create bundle
%
%..............................................................................

handle_create_bundle(ExamId) ->

	%
	% init
	%
	FsToSave = [
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


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


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

	%
	% init
	%
	ExamId = wf:q(id),
	OsmBundleId = wf:q(osm_bundle_fk),
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
	BundleDocs.



get_bgcolor(inwardstate, "completed", _, _) ->
	"bg-success";
get_bgcolor(scanningstate, "completed", [], _) ->
	"bg-danger";
get_bgcolor(scanningstate, "completed", "assigned", _) ->
	"bg-warning";
get_bgcolor(scanningstate, "completed", "completed", _) ->
	"bg-success";
get_bgcolor(uploadstate, "completed", "completed", []) ->
	"bg-danger";
get_bgcolor(uploadstate, "completed", "completed", "assigned") ->
	"bg-warning";
get_bgcolor(uploadstate, "completed", "completed", "completed") ->
	"bg-success";
get_bgcolor(_, _, _, _) ->
	[].


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
