
-module(dig_mm_ep_osm_apt).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("OSM Appointments").

heading() ->
	title().

form() ->
	ep_osm_apt.

digmm_actions() -> [
	?F(view, "View"),
	?F(edit, "Edit"),
	?F(pdf, "PDF"),
	?F(send, "Send")
].


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fs
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fs - group
%------------------------------------------------------------------------------

fields(ep_osm_apt, _Fs) ->
	ep_osm_apt:fs(basic).



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?ADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		mode=?VIEW,
		module=?MODULE,
		filters=ep_osm_apt:fs(basic),
		size=25,
		actions=[
			{send, "Send", "Send"}
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
fetch(D, From, Size, Fs) ->
	dig_mm:fetch(D, From, Size, Fs).



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	dig_mm:layout(?MODULE).



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event(send) ->
	handle_send_action();

event(send_confirmed) ->
	handle_send_confirmed();

event(E) ->
	dig_mm:event(E).

start_upload_event(Event) ->
	dig_mm:start_upload_event(Event).

finish_upload_event(Tag, AttachmentName, LocalFileData, Node) ->
	dig_mm:finish_upload_event(Tag, AttachmentName, LocalFileData, Node).

%------------------------------------------------------------------------------
% assertions
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% save
%------------------------------------------------------------------------------


%
% override before save function
%
before_save(FsToSave, _FsAll, _Doc) ->
	FsToSave.


%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - send action
%
%..............................................................................

handle_send_action() ->
	FSendCount = itf:textbox(?F(send_count, "Send Count")),
	FSendCount1 = FSendCount#field {
		validators=[required, integer, {
			"1-1000",
			validators:get(range_integer, 0, 1001)
		}]
	},
	Es = itl:get(?EDIT, [FSendCount1], ite:get(send_confirmed, "Send"), table),
	dig_mm:handle_show_action("Send Appointments", Es).



%..............................................................................
%
% handle - send confirmed
%
%..............................................................................

handle_send_confirmed() ->

	%
	% init
	%
	Dig = helper:state(dig),
	SendCount = ?S2I(wf:q(send_count)),
	Fs = dig:get_nonempty_fs(Dig),


	%
	% assert at least one filter is selected
	%
	?ASSERT(
		Fs /= [],
		"Please select at least one search filter"
	),


	%
	% get appointments to send
	%
	handle_send_confirmed(Fs, SendCount, 0),
	dig:log("Finished").


handle_send_confirmed(_Fs, SendCount, SentCount) when SentCount >= SendCount ->
	done;
handle_send_confirmed(Fs, SendCount, SentCount) ->


	%
	% init
	%
	{ok, TDoc} = ep_core_template_api:get_templatedoc_for_type("ep_osm_apt"),
	FetchCount = if
		SendCount < 100 -> SendCount;
		true -> 100
	end,


	%
	% get docs to send
	% send only new apts
	%
	Fs1 = itf:fs_merge(Fs, [
		itf:build(?OSMAPT(apt_state), ?NEW)
	]),
	Docs = ep_osm_apt_api:fetch(0, FetchCount, Fs1, [
		{use_index, ["apt_state"]}
	]),
	dig:log(itx:format("Found in this batch: ~p", [length(Docs)])),


	%
	% send apt
	%
	lists:foreach(fun(Doc) ->
		handle_send_apt(TDoc, Doc)
	end, Docs),


	%
	% recurse
	%
	case Docs of
		[] ->
			done;
		_ ->
			handle_send_confirmed(Fs, SendCount, SentCount + length(Docs))
	end.



%..............................................................................
%
% handle - send apt
%
%..............................................................................

handle_send_apt(TDoc, AppDoc) ->

	%
	% create pdf
	%
	{_Filename, Filepath} = ep_core_template_handler:handle_generate_pdf(TDoc, AppDoc),
	{ok, PDoc} = profiles:getdoc(itf:val(AppDoc, evaluator_id)),
	AptNumber = itf:val(AppDoc, apt_number),


	%
	% send mail
	% (From, ReplyTo, ToList, CCList, Subj, Body, FilePaths, ContentType)
	%
	Email = itf:val(PDoc, email),
	helper_mailer:send_mail_attachment_async(
		[],
		[Email],
		[Email],
		[],
		itx:format("CONFIDENTIAL: On-Screen Marking Appointment - ~s ", [AptNumber]),
		layout_letter_cover(),
		[Filepath],
		html
	),

	%
	% update state
	%
	AppId = itf:idval(AppDoc),
	FsToSave = [
		itf:build(?OSMAPT(apt_state), "sent")
	],
	{ok, _} = ep_osm_apt_api:save(FsToSave, ep_osm_apt:fs(all), AppId),



	Message = itx:format("Sent ~p", [AptNumber]),
	dig:log(Message).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

layout_letter_cover() ->
	Es = [
		#p {text="Respected Sir / Madam,"},
		#p {text="We're pleased to inform you that you have been appointed you as evaluator."},
		#p {text="Please find attached your appointment details."},
		#p {text="This email is system generated. Please do not reply to this e-mail."},
		#p {text="For any queries regarding appointment, please contact the university."},
		#p {text="Yours faithfully,"},
		#p {text="Controller of Examination"}
	],

	{ok, Html} = wf_render_elements:render_elements(Es),
	Html.

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
