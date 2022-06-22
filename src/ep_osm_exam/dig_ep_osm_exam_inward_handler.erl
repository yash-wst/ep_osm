-module(dig_ep_osm_exam_inward_handler).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-import(dig_ep_osm_exam_inward, [
	get_bundle_dir_name/2,
	sort_candidate_docs/1,
	assert_entry_does_not_exist_elsewhere/3,
	get_bundle_docs/0,
	redirect_to_main/0,
	redirect_to_main/0,
	redirect_to_main/0,
	sort_candidate_docs/1,
	sort_candidate_docs/1,
	get_bundle_docs/0,
	get_bundle_doc_from_cache/1,
	get_bundle_number_from_cache/1,
	get_bundle_doc_from_cache/1,
	get_bundle_number_from_cache/1,
	layout_candidate_edit/2,
	layout_candidate_remove/2
]).


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - discard bundle
%
%..............................................................................

handle_discard_bundle() ->

	%
	% init
	%
	ExamId = wf:q(osm_exam_fk),
	BundleId = wf:q(osm_bundle_fk),
	ExamDb = anpcandidates:db(ExamId),


	%
	% get candidate docs
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded")
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY, [{use_index, ["osm_bundle_fk"]}]
	),


	%
	% change candidate state to discarded
	%
	ListOfFsDoc = lists:map(fun(CDoc) ->
		FsDoc = helper_api:doc2fields({ok, CDoc}),
		itf:fs_merge(FsDoc, [
			fields:build(anp_paper_uid, itx:format("~s_discarded_~s", [
				itf:val(CDoc, anp_paper_uid), itf:idval(CDoc)
			])),
			fields:build(anpseatnumber, itx:format("~s_discarded_~s", [
				itf:val(CDoc, anpseatnumber), itf:idval(CDoc)
			])),
			fields:build(anpstate, "anpstate_discarded")
		])
	end, CandidateDocs),
	{ok, _} = anpcandidates:savebulk(ExamDb, ListOfFsDoc),


	%
	% change bundle number to discarded
	%
	FsToSaveBundle = [
		itf:build(?OSMBDL(inwardstate), "discarded")
	],
	{ok, _} = ep_osm_bundle_api:save(FsToSaveBundle, ep_osm_bundle:fs(all), BundleId),


	%
	% reload
	%
	helper:redirect(wf:uri()).


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
% handle - edit canddidate
%
%..............................................................................

handle_edit_candidate_dialog(CId) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(wf:q(osm_exam_fk)),
	{ok, CandidateDoc} = anpcandidates:getdoc(ExamDb, CId),



	%
	% layout 
	%
	Fs = itf:d2f(CandidateDoc, anpcandidate:fs(edit_receiver)),
	Fs1 = dig_mm:id_map(CId, Fs),
	Event = ite:get(?NID(edit_receiver, CId), "Save", {edit_receiver, CId}),
	Es = itl:get(?EDIT, Fs1, Event, table),


	%
	% show
	%
	itl:modal_fs(Es).



%..............................................................................
%
% handle - edit canddidate
%
%..............................................................................

handle_edit_candidate(CId) ->

	%
	% init
	%
	ExamId = wf:q(osm_exam_fk),
	ExamDb = anpcandidates:db(wf:q(osm_exam_fk)),
	{ok, CandidateDoc} = anpcandidates:getdoc(ExamDb, CId),
	Fs = anpcandidate:fs(edit_receiver),
	Fs1 = dig_mm:id_map(CId, Fs),
	Fs2 = itf:uivalue(Fs1),
	Fs3 = dig_mm:id_unmap(Fs2),


	%
	% vals
	%
	UId = helper:trim(itf:val2(Fs3, anp_paper_uid)),
	SNo = helper:trim(itf:val2(Fs3, anpseatnumber)),


	%
	% assert - entry does not exist in a different bundle
	%
	assert_entry_does_not_exist_elsewhere(UId, SNo, CandidateDoc),



	%
	% save
	%
	Changelist = itf:fs_changelist(CandidateDoc, Fs3),
	case Changelist of
		[] ->
			helper_ui:flash(warning, "No changes.");
		_ ->
			FComment = itf:d2f(CandidateDoc, fields:get(comments)),
			FComment1 = itf:build_comment(FComment, Changelist), 
			case ep_osm_candidate_api:update(ExamId, CandidateDoc, Fs3 ++ [FComment1]) of
				{ok, CandidateDoc1} ->
					helper:redirect(wf:uri()),
					helper_ui:flash(success, io_lib:format("Saved: ~s, ~s, ~s", [
						UId, SNo, itf:val(CandidateDoc1, anpfullname)
					]), 5);
				_ ->
					helper_ui:flash(error, io_lib:format("Error!: ~s, ~s", [UId, SNo]))
			end
	end,


	itl:modal_close().


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
			#dcell {val=itf:val(CDoc, anpfullname)},
			#dcell {val=itf:val(CDoc, total_pages)}
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
	CellClass = "font-weight-bold",
	Es1 = #table {
		class="table table-bordered table-sm table-condensed",
		rows=[
			#tablerow {
				cells=[
					#tablecell {body="Exam Season"},
					#tablecell {class=CellClass, body=SeasonName}
				]
			},
			#tablerow {
				cells=[
					#tablecell {body="CAP Centre"},
					#tablecell {class=CellClass, body=io_lib:format("(~s) ~s", [
						itf:val(CapDoc, code), itf:val(CapDoc, name)
					])}
				]
			},
			#tablerow {
				cells=[
					#tablecell {body="Faculty"},
					#tablecell {class=CellClass, body=ep_core_faculty_api:getname(
						itf:val(ExamDoc, faculty_code_fk)
					)}
				]
			},
			#tablerow {
				cells=[
					#tablecell {body="Program"},
					#tablecell {class=CellClass, body=ep_core_program_api:getname(
						itf:val(ExamDoc, program_code_fk)
					)}
				]
			},
			#tablerow {
				cells=[
					#tablecell {body="Subject"},
					#tablecell {class=CellClass, body=ep_core_subject_api:getname(
						itf:val(ExamDoc, subject_code_fk)
					)}
				]
			},
			#tablerow {
				cells=[
					#tablecell {body="Test Name"},
					#tablecell {class=CellClass, body=itf:val(ExamDoc, testname)}
				]
			},
			#tablerow {
				cells=[
					#tablecell {body="Course ID"},
					#tablecell {class=CellClass, body=itf:val(ExamDoc, anptestcourseid)}
				]
			},
			#tablerow {
				cells=[
					#tablecell {body="Bundle Number"},
					#tablecell {class=CellClass, body=itf:val(BundleDoc, number)}
				]
			}
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


	%
	% get result vals
	%
	{_, Results} = lists:foldl(fun(CDoc, {Slno, Acc}) ->
		UId = itf:val(CDoc, anp_paper_uid),
		SNo = itf:val(CDoc, anpseatnumber),
		{
			Slno + 1,
			Acc ++ [[
				?I2S(Slno),
				?CASE_IF_THEN_ELSE(SNo, [], UId, SNo)
			]]
		}
	end, {1, []}, CandidateDocs1),



	%
	% split into columns
	%
	ListOfResults = helper:list_split(Results, 25),
	Tables = lists:map(fun(NResults) ->
		Table = dig:layout_vals(#dig{config=[
			{action_layout_type, buttons},
			{show_slno, false},
			{responsive_type, scroll}
		]}, NResults, [
			"Sl. No.", "UID / Seat Number"
		]),
		Table1 = Table#table {class="table table-sm table-bordered"},
		layout:g(5, Table1)
	end, ListOfResults),
	Es2 = #table {
    	style="border-collapse: collapse;",
		class="table table-sm table-bordered",
		rows=[
			#tablerow {
				style="padding: 0px",
				cells=lists:map(fun(Table) ->
					#tablecell {
						style="padding: 0px",
						body=Table
					}
				end, Tables)
			}
		]
	},




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

handle_inward([], [], _) ->
	helper_ui:flash(error, "Please enter either barcode or student seat number", 5);

handle_inward(UId, SNo, TotalPages) ->

	%
	% assert total pages in int or empty
	%
	?ASSERT(
		(TotalPages ==[]) or (helper:l2i(TotalPages) /= error),
		"Total pages should be empty or an integer"
	),
	case UId of
		[] ->
			valid;
		_ ->
			validators:validate(UId, fields:get(anpseatnumber))
	end,
	case SNo of
		[] ->
			valid;
		_ ->
			validators:validate(SNo, fields:get(anpseatnumber))
	end,



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
	handle_inward(ExamId, OsmBundleId, UId, SNo, TotalPages, CandidateDocs).



handle_inward(ExamId, OsmBundleId, UId, SNo, TotalPages, []) ->
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
		itf:build(itf:textbox(?F(total_pages)), TotalPages),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded"),
		itf:build(itf:textbox(?F(timestamp_inward)), helper:epochtimestr())
	],
	case anpcandidates:save(ExamDb, FsToSave) of
		{ok, CandidateDoc} ->
			handle_inward_focus_textbox(),
			handle_insert_candidatedoc(BundleDoc, CandidateDoc),
			helper_ui:flash(success, io_lib:format("Created: ~s, ~s but could not find in master-data.", [UId, SNo]), 5);
		_ ->
			helper_ui:flash(error, io_lib:format("Error!: ~s, ~s", [UId, SNo]))
	end;


handle_inward(ExamId, OsmBundleId, UId, SNo, TotalPages, [Doc]) ->

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
		itf:build(itf:textbox(?F(total_pages)), TotalPages),
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
		#tablecell {body=[]},
		#tablecell {body=itf:val(CDoc, anp_paper_uid)},
		#tablecell {body=itf:val(CDoc, anpseatnumber)},
		#tablecell {body=?LN(?L2A(itf:val(CDoc, anpstate)))},
		#tablecell {body=itf:val(CDoc, anpfullname)},
		#tablecell {body=itf:val(CDoc, total_pages)},
		#tablecell {body=""},
		#tablecell {body=layout_candidate_edit(BundleDoc, CDoc)},
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
			helper_s3:aws_s3_bucket(),
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
% end
%------------------------------------------------------------------------------
