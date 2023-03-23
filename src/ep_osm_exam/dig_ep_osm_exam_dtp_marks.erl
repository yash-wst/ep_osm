
-module(dig_ep_osm_exam_dtp_marks).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-import(dig_ep_osm_exam_inward, [
	f/1
]).


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("DTP Marks Entry").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% fs
%------------------------------------------------------------------------------

fs(table) ->
	fields:getfields([
		anp_paper_uid,
		anpseatnumber,
		anp_paper_booklet_slno,
		anpfullname,
		dtp_marks_manual,
		dtp_marks_omr
	]);

fs(import) -> [
	itf:textbox(?F(id, "Exam Id"))
];

fs(dtp_marks_manual) ->
	FUId = fields:get(anp_paper_uid),
	FMarks = fields:get(dtp_marks_manual),
	[
		FUId#field {validators=[required]},	
		FMarks#field {validators=[required, number]}
	];
fs(dtp_marks_omr) -> [
	itf:attachment(?F(dtp_marks_omr_file, "OMR CSV File (UID,Marks)"))
].


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_PHYSICAL_INWARDER) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, ?APPOSM_SCANUPLOADER) -> true;
access(_, ?APPOSM_QC) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=[
			itf:build(f(osm_exam_fk), wf:q(id))
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	title().



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
fetch(D, _From, _Size, [
		#field {id=osm_exam_fk, uivalue=ExamId}
	]) ->

	%
	% init
	%
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),



	{
		D#dig {
			description=itx:format("~ts / ~ts", [
				itf:val(ExamDoc, anptestcourseid),
				itf:val(ExamDoc, testname)
			]),
			actions=[
				{dtp_marks_omr_form, "DTP Marks (OMR)", "DTP Marks (OMR)"},
				{dtp_marks_manual_form, "DTP Marks (Manual) ", "DTP Marks (Manual)"}
			],
			config=[
				{action_layout_type, buttons}
			]
		},
		[lists:map(fun(#field {label=Label}) ->
			#dcell {type=header, val=Label}
		end, fs(table))]
	};


%..............................................................................
%
% [other]
%
%..............................................................................
fetch(D, _From, _Size, _) ->
	{
		D,
		[{error, "This combination of filters has not been implemented.
		If you think it is useful, please contact the support team."}]
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
	event(dtp_marks_manual_form),
	handle_focus_textbox(),
	dig:dig(?MODULE:get()).



layout_dtp_marks_manual_form() ->
	Event = ite:get(update_dtp_marks_manual, "Update Marks", update_dtp_marks_manual),
	itl:get(?UPDATE, fs(dtp_marks_manual), Event, line).


layout_dtp_marks_omr_form() ->
	itl:get(?UPDATE, fs(dtp_marks_omr), noevent, table).

%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event(update_dtp_marks_manual) ->
	handle_update_dtp_marks_manual();

event(dtp_marks_manual_form) ->
	dig_mm:handle_show_action(
		"DTP Marks (Manual)",
		layout_dtp_marks_manual_form()
	);


event(dtp_marks_omr_form) ->
	dig_mm:handle_show_action(
		"DTP Marks (OMR)",
		layout_dtp_marks_omr_form()
	);

event({itx, E}) ->
	ite:event(E).


start_upload_event(Event) ->
	dig_mm:start_upload_event(Event).

finish_upload_event({_, dtp_marks_omr_file}, AttachmentName, LocalFileData, _Node) ->
	dig_mm_import:handle_finish_upload_event(
		?MODULE, ep_osm_candidate, ep_osm_candidate_api, 
		ep_osm_candidate_dtp_marks_omr_import,
		{file, AttachmentName, LocalFileData}
	);

finish_upload_event(Tag, AttachmentName, LocalFileData, Node) ->
	dig_mm:finish_upload_event(Tag, AttachmentName, LocalFileData, Node).

%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------

%..............................................................................
%
% handle - update dtp marks
%
%..............................................................................

handle_update_dtp_marks_manual() ->

	%
	% init
	%
	ExamId = wf:q(id),
	ExamDb = anpcandidates:db(ExamId),
	FsUi = itf:uivalue(fs(dtp_marks_manual)),

	%
	% get candidate doc
	%
	FsFind = [
		itf:build(itf:textbox(?F(anp_paper_uid)), wf:q(anp_paper_uid))
	],
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		ExamDb, FsFind, 0, 1, [
			{use_index, ["anp_paper_uid"]}
		]
	),
	handle_update_dtp_marks_manual(Docs, FsUi, ExamId).


%
% save
%
handle_update_dtp_marks_manual([Doc], FsUi, ExamId) ->

	%
	% asserts
	%
	CandidateState = itf:val(Doc, anpstate),
	MarksEntryEnabledStates = ep_osm_config:get_dtp_marks_enabled_states(),
	?ASSERT(
		lists:member(CandidateState, MarksEntryEnabledStates),
		itx:format("Error. Candidate state ~p. Required ~p", [
			?LN(?L2A(CandidateState)), MarksEntryEnabledStates
		])
	),


	%
	% init
	%
	UId = itf:val(Doc, anp_paper_uid),
	FsToSave = FsUi ++ [
		itf:build(fields:get(anpstate), "anpstate_completed")
	],
	Changelist = itf:fs_changelist(Doc, FsToSave),

	case Changelist of
		[] ->
			helper_ui:flash(warning, "No changes.");
		_ ->
			FComment = itf:d2f(Doc, fields:get(comments_dtp)),
			FComment1 = itf:build_comment(FComment, Changelist), 
			case ep_osm_candidate_api:update(ExamId, Doc, FsToSave ++ [FComment1]) of
				{ok, Doc1} ->
					handle_insert_candidatedoc(Doc1),
					helper_ui:flash(success, io_lib:format("Saved ~s: ~s vs. ~s", [
						itf:val(Doc1, anp_paper_uid),
						itf:val(Doc1, dtp_marks_manual),
						itf:val(Doc1, dtp_marks_omr)
					]), 5);
				Error ->
					?D(Error),
					helper_ui:flash(error, io_lib:format("Error!: ~s", [UId]))
			end
	end,


	%
	% final
	%
	handle_focus_textbox();



%
% not found
%
handle_update_dtp_marks_manual([], _FsUi, _ExamDb) ->
	Message = itx:format("~s not found!", [wf:q(anp_paper_uid)]),
	helper_ui:flash(error, Message, 5);

%
% error - multiple found 
%
handle_update_dtp_marks_manual(_, _FsUi, _ExamDb) ->
	helper_ui:flash(error, "Error!").



%
% focus
%
handle_focus_textbox() ->
	wf:wire("
		obj('anp_paper_uid').value = '';
		obj('dtp_marks_manual').value = '';
		obj('anp_paper_uid').focus();
		obj('anp_paper_uid').select();
	").


handle_insert_candidatedoc(CDoc) ->
	FsInward = fs(table),
	Row = #tablerow {
		cells=lists:map(fun(Fi) ->
			#tablecell {body=itf:val(CDoc, Fi#field.id)}
		end, FsInward)
	},
	wf:insert_top(dig:id(table), Row).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
