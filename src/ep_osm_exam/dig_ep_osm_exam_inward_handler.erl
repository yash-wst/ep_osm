-module(dig_ep_osm_exam_inward_handler).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-import(dig_ep_osm_exam_inward, [
	fs/1,
	get_bundle_dir_name/2,
	sort_candidate_docs/1,
	assert_entry_does_not_exist_elsewhere/3,
	redirect_to_main/0,
	get_bundle_docs/0,
	get_bundle_docs_count/0,
	get_bundle_doc_from_cache/1,
	get_bundle_number_from_cache/1,
	layout_candidate_edit/2,
	layout_candidate_remove/2,
	get_bundle_url/2
]).


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------

%..............................................................................
%
% handle - release bundle
%
%..............................................................................

handle_release_bundle({UserType, StateType, Id}) ->

	%
	% init
	%
	{ok, BundleDoc} = ep_osm_bundle_api:get(Id),


	%
	% assert
	%
	?ASSERT(
		itf:val(BundleDoc, UserType) == wf:user(),
		"Failed, this bundle is not assigned to you!"
	),



	%
	% fs to save
	%
	FsToSave = [
		itf:build(?OSMBDL(UserType), ""),
		itf:build(?OSMBDL(StateType), "")
	],


	%
	% save
	%
	case ep_osm_bundle_api:save(FsToSave, ep_osm_bundle:fs(all), Id) of
		{ok, _} ->
			wf:redirect(wf:uri());
		_ ->
			helper_ui:flash(error, "Sorry, could not save!")
	end.



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
	{ok, BundleDoc} = ep_osm_bundle_api:get(BundleId),


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
	FComment = itf:d2f(BundleDoc, ?OSMBDL(comments)),
	FsToSaveBundle = [
		itf:build(?OSMBDL(inwardstate), "discarded"),
		itf:build_comment(FComment, "Discarded")
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
	% Only Scanner who has the bundle assigned to himself can export folders
	%
	case itxauth:role() of
		?APPOSM_SCANUPLOADER ->
			ScanningPerson = itf:val(BundleDoc, scannedby),
			?ASSERT(
				ScanningPerson == itxauth:user(),
				"Unauthorised. This bundle is not assigned to you."
				);
		_ ->
			[]
	end,


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

handle_remove_candidate(CandidateId) ->

	%
	% init
	%
	ExamId = wf:q(osm_exam_fk),
	ExamDb = anpcandidates:db(ExamId),


	%
	% delete - but don't delete from master data
	%
	{ok, CandidateDoc} = anpcandidates:getdoc(ExamDb, CandidateId),
	ANPstate = itf:val2(CandidateDoc, anpstate),
	case ANPstate of
		"anpstate_expected" ->
			FsToSave = [
				fields:build(osm_bundle_fk, []),
				fields:build(timestamp_inward, [])
			],
			case ep_osm_candidate_api:update(ExamId, CandidateDoc, FsToSave) of
				{ok, _} ->
					dig:refresh();
				_ ->
					helper_ui:flash(error, "Error in updating master data.")
			end;

		_ ->
			case anpcandidates:delete(ExamDb, CandidateId) of
				{ok, _} ->
					dig:refresh();
				_ ->
					helper_ui:flash(error, "Sorry, could not delete!")
			end
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
	FIdsInward = dig_ep_osm_exam_inward:fids(inward),
	FsEditReceiver = itf:d2f(CandidateDoc, anpcandidate:fs(edit_receiver)),
	Fs = lists:map(fun(FId) ->
		itf:find(FsEditReceiver, FId)
	end, FIdsInward),
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

handle_inward_completed(ExamId, BundleId) ->

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
		itf:build(?OSMBDL(bundle_size), ?I2S(get_bundle_docs_count()))
	],

	%
	% validation if packet count does not match bundle inward size
	%
	validate_inward_count(BundleId),

	%
	% mark all anpstate as NU
    %
	ExamDb = anpcandidates:db(ExamId),

	%
	% 1. get all candidate docs of this bundle
	%
	FsToSearch = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_expected")
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearch, 0, ?INFINITY
	),

	%
	% 2. change candidate state from expected to not uploaded
	%
	ListOfFsDoc = lists:map(fun(CDoc) ->
		FsDoc = helper_api:doc2fields({ok, CDoc}),
		itf:fs_merge(FsDoc, [
			fields:build(anpstate, "anpstate_not_uploaded")
		])
	end, CandidateDocs),
	{ok, _} = anpcandidates:savebulk(ExamDb, ListOfFsDoc),


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
		Keys = helper_s3:get_keys(UploadedDir, [".jpg", ".pdf"]),


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
	AnpState = case ep_osm_config:is_qc_enabled() of
		true ->
			"anpstate_quality_check";
		false ->
			"anpstate_yettostart"
	end,
	ListOfFsDoc = lists:map(fun(CDoc) ->
		FsDoc = helper_api:doc2fields({ok, CDoc}),
		itf:fs_merge(FsDoc, [
			fields:build(anpstate, AnpState)
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
% handle - qc completed
%
%..............................................................................

handle_qc_completed(ExamId, BundleId) ->

	%
	% fs to save
	%
	FsToSave = [
		itf:build(?OSMBDL(qcstate), "completed"),
		itf:build(?OSMBDL(qc_date), helper:date_today_str())
	],


	%
	% mark all anpstate as NU
    %
	ExamDb = anpcandidates:db(ExamId),

	%
	% 1. get all candidate docs of this bundle
	%
	FsToSearch = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_quality_check")
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearch, 0, ?INFINITY, [
			{use_index, ["osm_bundle_fk"]}
		]
	),

	%
	% 2. change candidate state from expected to not uploaded
	%
	AnpState = dig_ep_osm_exam_inward:get_anpcandidate_state_after_qc_completed(ExamId),
	ListOfFsDoc = lists:map(fun(CDoc) ->
		FsDoc = helper_api:doc2fields({ok, CDoc}),
		itf:fs_merge(FsDoc, [
			fields:build(anpstate, AnpState)
		])
	end, CandidateDocs),
	{ok, _} = anpcandidates:savebulk(ExamDb, ListOfFsDoc),


	%
	% save
	%
	case ep_osm_bundle_api:save(FsToSave, ep_osm_bundle:fs(all), BundleId) of
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
	Fun = fun ({Type1, BundleDoc1, User1}) ->
		handle_assign_bundle1(Type1, BundleDoc1, User1)
	end,
	case mini_task_queue:now(Fun, {Type, BundleDoc, User}) of
		{ok, Res} ->
			case Res of
				{ok, Doc} ->
					ExamId = itf:val(Doc, osm_exam_fk),
					BundleId = itf:idval(BundleDoc),
					dig_ep_osm_exam_inward:redirect_to_bundle(ExamId, BundleId);
				_ ->
					helper_ui:flash(error, "Sorry, could not assign!")
			end;
		{error, [mini_task_queue, throw, {Error, Message}]} ->
			throw({Error, Message})
	end.



handle_assign_bundle1(Type, BundleDoc, User) ->

	%
	% init
	%
	StateFId = case Type of
		createdby ->
			inwardstate;
		scannedby ->
			scanningstate;
		qualityby ->
			uploadstate;
		qcby ->
			qcstate
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
		(
			(itf:val(Doc, Type) == []) or
			(itf:val(Doc, Type) == ?NEW)
		),
		"ERROR: This bundle is already assigned!"
	),


	%
	% assert - only X bundles at max can be self assigned for scanning
	% or uploading state.
	%

	check_max_limits(Type, StateFId, User),


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
	ep_osm_bundle_api:save(FsAll2).



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
			},
			#tablerow {
				cells=[
					#tablecell {body="Packet Number"},
					#tablecell {class=CellClass, body=itf:val(BundleDoc, packet_number)}
				]
			},
			#tablerow {
				cells=[
					#tablecell {body="Rack Location"},
					#tablecell {class=CellClass, body=itf:val(BundleDoc, rack_location)}
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


handle_inward(UId, SNo, _TotalPages) ->

	%
	% init
	%
	SNo1 = ?CASE_IF_THEN_ELSE(SNo, [], UId, SNo),
	FsInward = dig_ep_osm_exam_inward:fs({inward, []}),
	FsInwardUi = itf:fs_merge(itf:uivalue(FsInward), [
		itf:build(fields:get(anpseatnumber), SNo1)
	]),


	%
	% validate inward fs
	%
	handle_validate_inward_fs(itf:uivalue(FsInward)),


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
	BundleDocsCount = get_bundle_docs_count(),
	BundleSize = itxconfigs_cache:get2(ep_osm_exam_inward_bundle_size, 60),
	?ASSERT(
		BundleDocsCount < (BundleSize + 1),
		"bundle full; create new bundle"
	),



	%
	% search for existing docs
	%
	CandidateDocs = get_existing_candidate_docs(ExamDb, UId, SNo1),
	handle_inward(ExamId, OsmBundleId, FsInwardUi, CandidateDocs).


%
% inward seat number which does not exist in previously imported master data
%
handle_inward(ExamId, OsmBundleId, FsInwardUi, []) ->

	%
	% init
	%
	UId = itf:val2(FsInwardUi, anp_paper_uid),
	SNo = itf:val2(FsInwardUi, anpseatnumber),



	%
	% create entry in exam db
	%
	ExamDb = anpcandidates:db(ExamId),
	BundleDoc = get_bundle_doc_from_cache(OsmBundleId),
	BundleNumber = get_bundle_number_from_cache(OsmBundleId),



	FsToSave = FsInwardUi ++ [
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId),
		itf:build(itf:textbox(?F(anpcentercode)), "B:" ++ BundleNumber),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded"),
		itf:build(itf:textbox(?F(timestamp_inward)), new_timestamp_inward(ExamDb, OsmBundleId)),
		itf:build(itf:textbox(?F(master_data_status)), "not_matched")
	],
	case anpcandidates:save(ExamDb, FsToSave) of
		{ok, CandidateDoc} ->
			handle_inward_focus_textbox(),
			handle_insert_candidatedoc(BundleDoc, CandidateDoc),
			helper_ui:flash(warning, io_lib:format("Created: ~s, ~s but could not find in master-data.", [UId, SNo]), 5);
		_ ->
			helper_ui:flash(error, io_lib:format("Error!: ~s, ~s", [UId, SNo]))
	end;

%
% inward student seat number which is already in master data
%
handle_inward(ExamId, OsmBundleId, FsInwardUi, [Doc]) ->

	%
	% init
	%
	UId = case itf:val(Doc, anp_paper_uid) of
		[] ->
			itf:val2(FsInwardUi, anp_paper_uid);
		UId0 ->
			UId0
	end,
	SNo = case itf:val(Doc, anpseatnumber) of
		[] ->
			itf:val2(FsInwardUi, anpseatnumber);
		SNo0 ->
			SNo0
	end,
	ExamDb = anpcandidates:db(ExamId),
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
	FsInwardUi1 = itf:fs_delete(FsInwardUi, fields:getfields([
		anpseatnumber
	])),
	FsToSave = FsInwardUi1 ++ [
		itf:build(itf:textbox(?F(osm_bundle_fk)), OsmBundleId),
		itf:build(itf:textbox(?F(anpstate)), "anpstate_not_uploaded"),
		itf:build(itf:textbox(?F(timestamp_inward)), new_timestamp_inward(ExamDb, BundleId)),
		itf:build(itf:textbox(?F(master_data_status)), "matched")
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
	end;


handle_inward(_ExamId, _OsmBundleId, _FsInwardUi, _Docs) ->
	helper_ui:flash(error, "Multiple documents were found!").



handle_inward_focus_textbox() ->
	UId = wf:q(anp_paper_uid),
	SNo = wf:q(anpseatnumber),

	wf:wire("
		obj('anp_paper_uid').value = '';
		obj('anpseatnumber').value = '';
	"),

	if
		UId /= [] ->
			wf:wire("
				obj('anp_paper_uid').focus();
				obj('anp_paper_uid').select();
			");
		SNo /= [] ->
			wf:wire("
				obj('anpseatnumber').focus();
				obj('anpseatnumber').select();
			");
		true ->
			ok
	end.



handle_insert_candidatedoc(BundleDoc, CDoc) ->
	FsInward = fs(table),
	Row = #tablerow {cells=[
		#tablecell {body=[]}
	] ++
	lists:map(fun(Fi) ->
		#tablecell {body=itf:val(CDoc, Fi#field.id)}
	end, FsInward)
	++ [
		#tablecell {body=helper:epochstrtotime(itf:val(CDoc, timestamp_inward))},
		#tablecell {body=?LN(?L2A(itf:val(CDoc, anpstate)))},
		#tablecell {body=itf:val(CDoc, anpfullname)},
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

handle_create_bundle(ExamId, PacketNumber, RackLocation, PacketCount) ->

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
		itf:build(?OSMBDL(inward_date), ""),
		itf:build(?OSMBDL(scanned_date), ""),
		itf:build(?OSMBDL(uploaded_date), ""),
		itf:build(?OSMBDL(qc_date), ""),
		itf:build(?OSMBDL(inwardstate), "new"),
		itf:build(?OSMBDL(scanningstate), ""),
		itf:build(?OSMBDL(uploadstate), ""),
		itf:build(?OSMBDL(qcstate), ""),
		itf:build(?OSMBDL(packet_number), PacketNumber ),
		itf:build(?OSMBDL(packet_count), PacketCount ),
		itf:build(?OSMBDL(rack_location), RackLocation)
	] ++ case itxauth:role() of
		?APPOSM_PHYSICAL_INWARDER ->[
			itf:build(?OSMBDL(receivedby), itxauth:user()),
			itf:build(?OSMBDL(receivedon), helper:epochtimestr()),
			itf:build(?OSMBDL(createdby), ""),
			itf:build(?OSMBDL(createdon), "")
			];
		?APPOSM_RECEIVER -> [
			itf:build(?OSMBDL(receivedby), ""),
			itf:build(?OSMBDL(receivedon), ""),
			itf:build(?OSMBDL(createdby), itxauth:user()),
			itf:build(?OSMBDL(createdon), helper:epochtimestr())
			];
		_ -> []
		end,


	%
	% save
	%
	{ok,  {ok, BundleDoc}} = ep_osm_bundle_api:create(FsToSave),
	helper:redirect(get_bundle_url(ExamId, itf:idval(BundleDoc))).




%..............................................................................
%
% handle - create bundle form -
%
% shows confirmation popup and input textboxes for packet number and rack
% location
%
%..............................................................................

handle_create_bundle_form() ->

	%
	% init
	%
	Fs = [
		?OSMBDL(packet_number),
		?OSMBDL(packet_count),
		?OSMBDL(rack_location)
	],


	%
	% show dialog
	%
	Es = itl:get(?CREATE, Fs, ite:get(create_bundle, "Create Bundle"), table),
	itl:modal_fs(itl:section("Create a new bundle", Es)).


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
% misc
%------------------------------------------------------------------------------


new_timestamp_inward(ExamDb, BundleId) ->

	%
	% init
	%
	EpochTime = helper:epochtimestr(),


	%
	% find docs
	%
	FsToSearchBundle = [
		itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId),
		itf:build(itf:textbox(?F(timestamp_inward)), EpochTime)
	],
	Db2FindRec = db2_find:getrecord_by_fs(
		ExamDb, FsToSearchBundle, 0, 10, [
			{use_index, ["osm_bundle_fk"]}
		]
	),
	#db2_find_response {docs=Docs} = db2_find:find(Db2FindRec#db2_find {
		fields=[
			itf:textbox(?F(timestamp_inward))
		]
	}),


	%
	% return epoch time
	%
	case Docs of
		[] ->
			EpochTime;
		_ ->
			LastDoc = lists:last(sort_candidate_docs(Docs)),
			NewEpochtime = helper:s2i(itf:val(LastDoc, timestamp_inward)) + 1,
			helper:i2s(NewEpochtime)
	end.



%..............................................................................
%
% check max limits
%
%..............................................................................

check_max_limits(Type, DB_state_Key, User) when Type /= qcby, Type /= createdby ->

	Settings_key = case Type of
		scannedby ->
			ep_osm_max_bundles_in_scanning_state_allowed;
		qualityby ->
			ep_osm_max_bundles_in_uploading_state_allowed
		end,

	Search_Index = atom_to_list(Type),

	MaxBundleCount = itxconfigs_cache:get2(Settings_key, 2),

	FsFind = [
			itf:build(?OSMBDL(Type), User),
			itf:build(?OSMBDL(DB_state_Key), "assigned")
	],

	Docs = ep_osm_bundle_api:fetch(
				0,
				MaxBundleCount,
				FsFind,
				[{use_index, [Search_Index] }]
	),

	Count = length(Docs),

	Message = case Type of
		scannedby -> scanning;
		qualityby -> uploading
	end,

	?ASSERT(
		Count < MaxBundleCount,
		itx:format("You cannot self assign more than ~p bundles in the ~p state",
			[MaxBundleCount, Message])
	);

check_max_limits(_, _, _) ->
	ok.

%..............................................................................
%
% check if inward count is same as packet count for bundle
%
%..............................................................................

validate_inward_count(BundleId) ->
	{ok, BundleDoc} = ep_osm_bundle_api:get(BundleId),
	PacketCount_Str = itf:val(BundleDoc, packet_count),
	InwardCount = get_bundle_docs_count(),

	%
	% some bundles were created without Packet Count,
	% do not run validation for such bundles
	%
	case PacketCount_Str of
		[] -> ok;
		_ ->
			PacketCount_Int = helper:s2i(PacketCount_Str),
			?ASSERT(
				InwardCount == PacketCount_Int,
				itx:format("Booklet Inward Count ~p does not match Packet Count ~p",
					[InwardCount, PacketCount_Int])
			)
	end.



%..............................................................................
%
% handle - validate inward fs
%
%..............................................................................

handle_validate_inward_fs(Fs) ->
	%
	% for each field
	%
	lists:foreach(fun(#field {label=Label, uivalue=Val, validators=Validators}) ->

		%
		% for each validator of the field
		%
		lists:foreach(fun(ValidatorId) ->

			%
			% validate
			%
			ValidatorFn = validators:get(ValidatorId),
			ValidatorTip = validators:tip(ValidatorId),
			?ASSERT(
				ValidatorFn(undefined, Val),
				itx:format("~ts: ~ts", [Label, ValidatorTip])
			)

		end, Validators)
	end, Fs).




%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% get existing candidate docs
%
get_existing_candidate_docs(Db, UId, SeatNumber) ->

	%
	% get candidates by UID
	%
	DocsUId = getdoc_by_id(Db, anp_paper_uid, UId),


	%
	% get docs by seat number
	%
	DocsSeatNumbers = getdoc_by_id(Db, anpseatnumber, SeatNumber),


	%
	% return unique
	%
	helper:unique(DocsUId ++ DocsSeatNumbers).



%
% get doc by id
%
getdoc_by_id(_Db, _FId, []) ->
	[];
getdoc_by_id(Db, FId, Val) ->
	FsFind = [
		itf:build(itf:textbox(?F(FId)), Val)
	],
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(Db, FsFind, 0, ?INFINITY, [
		{use_index, [?A2L(FId)]}
	]),
	Docs.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
