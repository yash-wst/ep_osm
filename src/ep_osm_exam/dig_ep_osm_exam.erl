
-module(dig_ep_osm_exam).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("OSM Exams").

heading() ->
	title().

form() ->
	ep_osm_exam.


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------

-define(BATCH_SIZE, 100).


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_ANPADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=anptest:fs(search),
		size=25,
		events=[
			ite:button(export, "CSV", {itx, {dig, export}})
		],
		actions=[
			{export_exam_folders, "Export Exam Folders", "Export Exam Folders"},
			{action_import_from_rps, "Import - Create exams from RPS", "Import - Create exams from RPS"},
			{action_import, "Import - Create exams from CSV", "Import - Create exams from CSV"},
			{action_import_student_data, "Import Student Data", "Import Student Data"},
			{action_uploadzip, "Upload Zip - question paper, model answer PDF.", "Upload Zip - question paper, model answer PDF."},
			{action_change_state, "Change State", "Change State"}
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exams").



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

	%
	% fetch documents from db
	%
	Rec = db2_find:getrecord_by_fs(anptests:getdb(), Fs, From, Size),
	#db2_find_response {docs=Docs}  = db2_find:find(
		Rec#db2_find {sort=anptest:fs(search)}
	),



	%
	% build dicts
	%
	{SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict} =
		ep_core_helper:get_sfps_dicts(Docs),

	%
	% layout results
	%
	Results = lists:map(fun(Doc) ->

		%
		% init
		%
		FsDoc = itf:d2f(Doc, anptest:fs(form)),
		FsIndex = itf:d2f(Doc, anptest:fs(index)),


		%
		% sfps cells
		%
		SFPSCells = ep_core_dig_helper:get_sfps_cells(
			Doc, {SeasonDocsDict, FacultyDocsDict, ProgramDocsDict, SubjectDocsDict},
			#dcell {show_ui=false}
		),


		%
		% layout
		%
		SFPSCells ++ lists:map(fun(F) ->
			#dcell {val=itl:render(F)}
		end, FsDoc) ++ [
			#dcell {
				show_csv=false,
				val=layout_files(Doc)}
		] ++ [
			#dcell {
				show_csv=false,
				val=helper_ui:layout_slinks(anptest, FsIndex)
			}
		]

	end, Docs),


	%
	% header
	%
	Header = [
		#dcell {type=header, show_ui=false, val="Season"},
		#dcell {type=header, show_ui=false, val="Faculty"},
		#dcell {type=header, show_ui=false, val="Program"},
		#dcell {type=header, show_ui=false, val="Subject"}
	] ++ lists:map(fun(#field {label=Label}) ->
		#dcell {type=header, val=Label}
	end, anptest:fs(form)) ++ [
		#dcell {type=header, show_csv=false, val="Files"},
		#dcell {type=header, show_csv=false, val="Actions"}

	],

	{
		D#dig {
			total=?INFINITY
		},
		[Header] ++ Results
	}.


%------------------------------------------------------------------------------
% function - exports
%------------------------------------------------------------------------------
exports() -> [
].



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	handle_objectkey(wf:q(objectkey)),
	dig:dig(?MODULE:get()).



%..............................................................................
%
% layout - change state
%
%..............................................................................

layout_change_state() ->
	F = fields:get(teststatus),
	Fs = [
		F#field {id=to_state, label="To State"}
	],
	Es = itl:get(?CREATE, Fs, ite:get(change_state, "Change State"), table),
	dig_mm:handle_show_action("Change State", Es).


%..............................................................................
%
% layout - files
%
%..............................................................................

layout_files(Doc) ->

	%
	% init
	%
	Names = attachment:get_names(Doc),


	%
	% layout
	%
	lists:map(fun(Name) ->
		#button {
			style="display:block;",
			class="btn btn-link",
			text=Name,
			postback={download, itf:idval(Doc), Name}
		}
	end, Names).



%..............................................................................
%
% layout - import student data form
%
%..............................................................................

layout_import_student_data() ->
	itl:get(?EDIT, ep_osm_exam:fs(import_student_data), noevent, table).


%..............................................................................
%
% layout - upload form
%
%..............................................................................

layout_upload_form() ->

	%
	% init
	%
	SeasonId = wf:q(season_fk),
	?ASSERT(
		((SeasonId /= []) and (SeasonId /= undefined)),
		"ERROR: Please select a season under which files are to be uploaded"
	),


	ObjectKey = ?FLATTEN(io_lib:format("~s_~s.zip", [
		SeasonId,
		helper:uidintstr()
	])),

	RedirectUrl = ?FLATTEN(io_lib:format("~s/~p?objectkey=~s", [
		customer:get(mainserver_url),
		?MODULE,
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
	[
		itl:instructions([
			{danger, "Folder name and zip file name should be same. Ex: Folder: 200, Zip: 200.zip"},
			{ok, "Zip file should contain folders named by course ID"},
			{ok, "Smaller PDF is assumed to be question paper and the bigger file is model answer."},
			{danger, "Existing files will be overwritten!"}
		]),
		Es
	].


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(action_import_from_rps) ->
	helper:redirect("/dig_mm_ep_osm_exam_from_frp");

event({browser_to_s3_completed, ObjectKey}) ->
	handle_objectkey_upload_completed(ObjectKey);


event({confirmation_yes, change_state}) ->
	itl:modal_close(),
	handle_change_state_confirmed();

event(change_state) ->
	handle_change_state();

event(action_change_state) ->
	layout_change_state();

event(action_import_student_data) ->
	dig_mm:handle_show_action("Import Student Data", layout_import_student_data());

event(action_uploadzip) ->
	dig_mm:handle_show_action("Upload Zip", layout_upload_form());

event({download, DocId, AttachmentName}) ->
	attachment:download(anptests:getdb(), DocId, AttachmentName);

event(export_exam_folders) ->
	handle_export_exams_dir();

event(E) ->
	dig_mm:event(E).

start_upload_event(Event) ->
	dig_mm:start_upload_event(Event).

finish_upload_event({_,file_import_student_data}, AttachmentName, LocalFileData, _Node) ->
	SeasonId = wf:q(import_season_fk),
	?ASSERT(
		((SeasonId /= []) and (SeasonId /= undefined)),
		"ERROR: Please select a season under which file is to be uploaded"
	),
	dig_mm_import:handle_finish_upload_event(
		?MODULE, ep_osm_candidate, ep_osm_candidate_api, ep_osm_candidate_import,
		{file, AttachmentName, LocalFileData}
	);

finish_upload_event(Tag, AttachmentName, LocalFileData, Node) ->
	dig_mm:finish_upload_event(Tag, AttachmentName, LocalFileData, Node).


%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle export exams dir
%
%..............................................................................


handle_export_exams_dir() ->

	%
	% init
	%
	FsFind = dig:get_nonempty_fs(helper:state(dig)),
	?ASSERT(
		FsFind /= [],
		"Please select at least one filter"
	),

	%
	% init
	%
	RootDir = helper:create_unique_dir(),

	%
	% create dirs in batches
	%
	handle_export_exams_dir(0, RootDir, FsFind),

	%
	% zip the root dir
	%
	{Filename, Filepath} = helper:zip_clean_dir(RootDir),
	itxdownload:stream(Filename, Filepath).


handle_export_exams_dir(From, RootDir, FsFind) ->
	%
	% get matching exam docs in batches
	%
	ExamDocs = ep_osm_exam_api:fetch(From, ?BATCH_SIZE, FsFind),

	%
	% create directory for each exam doc
	%
	lists:foreach(fun(ExamDoc) ->
		handle_export_exam_dir(RootDir, ExamDoc)
	end , ExamDocs),


	case length(ExamDocs) < ?BATCH_SIZE of
		true ->
			done;
		_ ->
			handle_export_exams_dir(From + ?BATCH_SIZE, ?BATCH_SIZE, FsFind)
	end.


handle_export_exam_dir(RootDir, ExamDoc) ->
	%
	% init
	% get faculty, program, subject documents
	%
	SeasonId = itf:val(ExamDoc, season_fk),
	FacultyId = itf:val(ExamDoc, faculty_code_fk),
	ProgramId = itf:val(ExamDoc, program_code_fk),
	SubjectId = itf:val(ExamDoc, subject_code_fk),

	SeasonDoc = ep_core_exam_season_api:get_from_cache(SeasonId),
	FacultyDoc = ep_core_faculty_api:get_from_cache(FacultyId),
	ProgramDoc = ep_core_program_api:get_from_cache(ProgramId),
	SubjectDoc = ep_core_subject_api:get_from_cache(SubjectId),

	SeasonName = helper:sanitisestr(itf:val(SeasonDoc, name)),
	FacultyName = helper:sanitisestr(itf:val(FacultyDoc,faculty_name)),
	ProgramName = helper:sanitisestr(itf:val(ProgramDoc,program_name)),
	SubjectName = helper:sanitisestr(itf:val(SubjectDoc,subject_name)),
	ExamName = helper:sanitisestr(itf:val(ExamDoc, testname)),

	%
	% create dir
	%
	Dir = itx:format("~s/~ts/~ts/~ts/~ts/~ts/", [
		RootDir, SeasonName, FacultyName, ProgramName, SubjectName, ExamName
	]),
	helper:cmd("mkdir -p ~ts", [Dir]).

%..............................................................................
%
% handle change state
%
%..............................................................................

handle_change_state() ->
	%
	% get tests
	%
	Docs = get_tests_change_state(0, ?BATCH_SIZE),


	?ASSERT(
		Docs /= [],
		"Could not find any matching test documents"
	),


	%
	% confirmation
	%
	Count = length(Docs),
	Message = if
		Count < ?BATCH_SIZE ->
			itx:format("There are ~p tests to update. Are you sure you want continue?", [Count]);
		true ->
			itx:format("There are more than ~p tests to update. Are you sure you want to continue?", [
				?BATCH_SIZE
			])
	end,
	itl:confirmation(Message, change_state).






%..............................................................................
%
% handle change state confirmed
%
%..............................................................................

handle_change_state_confirmed() ->

	%
	% init
	%
	handle_change_state_confirmed(get_tests(0, ?BATCH_SIZE)).


handle_change_state_confirmed(Docs) ->

	%
	% init
	%
	ToState = wf:q(to_state),
	FsToSave = [
		fields:build(teststatus, ToState)
	],


	%
	% update date
	%
	DocsToSave = lists:map(fun(Doc) ->
		Fs = helper_api:doc2fields({ok, Doc}),
		Fs1 = itf:fs_merge(Fs, FsToSave),
		helper_api:fields2doc(Fs1)
	end, Docs),


	%
	% save
	%
	{ok, ResDocs} = db:savebulk(anptests:getdb(), DocsToSave),
	Message = itx:format("Batch save result {ok, error}: ~p", [db_helper:bulksave_summary(ResDocs)]),
	dig:log(warning, Message),


	%
	% recurse
	%
	case length(Docs) == ?BATCH_SIZE of
		true ->
			handle_change_state_confirmed(get_tests(0, ?BATCH_SIZE));
		_ ->
			dig:log(success, "Finished"),
			over
	end.





%..............................................................................
%
% handle - object key upload completed
%
%..............................................................................

handle_objectkey_upload_completed(ObjectKey) ->
	dig_ep_osm_exam_file_upload:upload(ObjectKey).


%..............................................................................
%
% handle - object key
%
%..............................................................................


handle_objectkey(ObjectKey) when ObjectKey /=[], ObjectKey /= undefined ->
	%
	% before processing check if object exists
	%
	try
		_Infos = helper_s3:info_dir(
			helper_s3:aws_s3_bucket(),
			"browser_to_s3/" ++ ObjectKey
		),

		wf:wire(#event{type=timer, delay=100, postback={browser_to_s3_completed, ObjectKey}}),
		#span {text=[]}

	catch
		_E:_M ->
			skip
	end;

handle_objectkey(_) ->
	ok.



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


get_tests(From, Size) ->
	%
	% init
	%
	Dig = helper:state(dig),
	Fs = dig:get_nonempty_fs(Dig#dig.filters),


	?ASSERT(
		Fs /= [],
		"Please select at least one filter and then try again"
	),


	%
	% get tests
	%
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		ep_osm_exam_api:db(), Fs, From, Size
	),
	Docs.



%
% get tests - change state
%
get_tests_change_state(From, Size) ->
	%
	% init
	%
	Dig = helper:state(dig),
	Fs = dig:get_nonempty_fs(Dig#dig.filters),


	?ASSERT(
		Fs /= [],
		"Please select at least one filter and then try again"
	),

	?ASSERT(
		itf:find(Fs, teststatus) /= undefined,
		"Please select test status. It cannot be empty"
	),


	?ASSERT(
		itf:val2(Fs, teststatus) /= wf:q(to_state),
		"Test status and To State cannot be same"
	),


	%
	% get tests
	%
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		ep_osm_exam_api:db(), Fs, From, Size
	),
	Docs.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
