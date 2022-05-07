-module(ep_osm_verify_images).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).


title() ->
	?LN("Evaluation View").

heading() ->
	?LN("Evaluation View").



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------

access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, ?APPOSM_SCANUPLOADER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------

layout() ->

	%
	% init
	%
	TestId = wf:q(anptestid),
	CandidateId = wf:q(anpid),
	TFs = anptests:get(TestId),
	Fs = anpcandidates:get(anpcandidates:db(TestId), CandidateId),
	ImgUrls = anpcandidate:get_image_urls(fields:getuivalue(TFs, aws_s3_dir), TFs, Fs),


	%
	% layout
	%
	?AKIT({layout, card_list_group, [
		layout_student_info(TFs, Fs),
		layout_upload(TFs, Fs),
		layout_page_nos(TFs, Fs, ImgUrls),
		layout_answerpaper(TFs, Fs, ImgUrls)
	]}).





%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------



%
% layout - student info
%

layout_student_info(_TFs, Fs) ->
	Es = layout:get(?VIEW, fields:getfields(Fs, [
		anpseatnumber,
		anpfullname,
		anpstate
	]), [], table),
	[
		#p {class="font-weight-bold", text="Student Info"},
		Es
	].


%
% layout - page numbers
%

layout_page_nos(_TFs, _Fs, ImgUrls) ->
	{Es, _} = lists:foldl(fun(ImgUrl, {Acc, Index}) ->
		AName = anpcandidate:get_aname_from_imgurl(ImgUrl),
		Link = #link {
			class="btn btn-primary-outline m-1",
			text=itx:format("~p (~s)", [Index, AName]),
			url="#" ++ AName
		},
		{
			Acc ++ [
				layout:g(3, Link)
			],
			Index+1
		}
	end, {[], 1}, ImgUrls),
	[
		#p {class="font-weight-bold", text="Pages"},
		layout:grow(Es)
	].




%
% layout answer paper images
%
layout_answerpaper(_TFs, _Fs, ImgUrls) ->
	%
	% layout images
	%
	lists:map(fun(ImageUrl) ->
		AName = anpcandidate:get_aname_from_imgurl(ImageUrl),
		[
			lists:flatten(io_lib:format("
				<a id='~s' href='#'></a>", [AName]
			)),
			#p {
				class="bg-info p-2 mt-3",
				text=AName
			},
			#image {
				class="border border-2 border-primary",
				style="width: 100%",
				image=ImageUrl
			}
		]
	end, ImgUrls).



%
% layout - upload
%
layout_upload(TFs, Fs) ->
	layout_upload(TFs, Fs, itf:val(Fs, anpstate)).


layout_upload(_TFs, Fs, "anpstate_discarded") -> [
	ite:button(
		move_to_yet_to_start,
		"Move to Yet-to-start",
		move_to_yet_to_start,
		"btn btn-sm btn-danger-outline pull-sm-right"
	),
	itl:get(?EDIT, [itf:attachment(
		?F({seatnumber_zip, itf:val(Fs, anpseatnumber)}, "Upload Zip file (SEATNUMBER.zip)"))
	], noevent, table)
];
layout_upload(_TFs, _Fs, _) ->
	[].



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({confirmation_yes, move_to_yet_to_start}) ->
	handle_move_to_yet_to_start(wf:q(anptestid), wf:q(anpid));

event(move_to_yet_to_start) ->
	itl:confirmation(#panel {class="mycenter", body=[
		#p {text="Have you fixed the problem with images?"},
		#p {text="Do you want to change state from Discarded to Yet To Start?"}
	]}, move_to_yet_to_start);
		 
event(Event) ->
	?D(Event).



%------------------------------------------------------------------------------
% event - file
%------------------------------------------------------------------------------
start_upload_event({attachment_upload1, _}) ->
	helper_ui:flash(?LN("Uploading. Please Wait ...")).


finish_upload_event(
	{attachment_upload1, {seatnumber_zip, SeatNumber}},
	AttachmentName,
	LocalFileData,
	_Node
) ->

	dig_ep_osm_exam_inward_uploadtos3:handle_upload_seatnumber_zip(
		wf:q(anptestid),  wf:q(anpid), SeatNumber, AttachmentName, LocalFileData
	),
	helper:redirect(wf:uri()).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% handle
%------------------------------------------------------------------------------

handle_move_to_yet_to_start(ExamId, CandidateId) ->

	%
	% reset candidate
	%
	anpcandidate:handle_anpcandidate_reset(ExamId, CandidateId),


	%
	% init
	%
	ExamDb = anpcandidates:db(ExamId),
	{ok, CandidateDoc} = anpcandidates:getdoc(ExamDb, CandidateId),


	%
	% fs to save
	%
	FsToSave = [
		fields:build(anpstate, "anpstate_yettostart")
	],
	Changelist = itf:fs_changelist(CandidateDoc, FsToSave),
	FComment = itf:d2f(CandidateDoc, fields:get(comments)),
	FComment1 = itf:build_comment(FComment, Changelist), 


	%
	% save
	%
	{ok, _} = ep_osm_candidate_api:update(ExamId, CandidateDoc, FsToSave ++ [FComment1]),
	helper:redirect(wf:uri()).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
