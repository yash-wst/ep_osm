
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
		[]
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
	dig:dig(?MODULE:get()).



layout_dtp_marks_manual_form() ->
	Event = ite:get(update_dtp_marks_manual, "Update Marks", update_dtp_marks_manual),
	itl:get(?UPDATE, fs(dtp_marks_manual), Event, table).


layout_dtp_marks_omr_form() ->
	itl:get(?UPDATE, fs(dtp_marks_omr), noevent, table).

%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

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



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
