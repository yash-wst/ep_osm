-module(ep_osm_eval_v2_event_handler).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%..............................................................................
%
% event handler
%
%..............................................................................

%
% handle noevent
%
event(noevent) ->
	[];


%
% add comment event
%
event({add_remark}) ->
	Res = anpcandidate:addcomment(
			wf:q(anptest:id()),
			wf:q(anpcandidate:id()),
			anpcandidate:get_comment_key(),
			wf:q(txtarea_remarks_id)
		),
	ep_osm_eval_v2_remarks:layout_remarks_update(helper_api:doc2fields(Res));


%
% event for page nav dropdown
%
event({page_nav_dropdown, {page_nos, Index, AName}})->
	wf:wire("$('#navbar_page_no').dropdown('toggle')"),
	wf:redirect("#" ++ AName),
	anpcandidate:event({page_nos, Index, AName});


%-------------------------------------------------------------------------------
%
% modal related events
%
%-------------------------------------------------------------------------------

%
% skip evaluation modal
%
event({skip_eval_event}) ->
	ep_osm_eval_v2_modals:modal_skip_evaluation_confirmation();

event({close_skip_eval_modal}) ->
	itl:modal_close(),
	event({reject_answerpaper, no});

event({reject_answerpaper, reject}) ->
	ep_osm_eval_v2_modals:modal_skip_evaluation_final();

event(confirm_reject) ->
	TId = wf:q(anptest:id()),
	CId = wf:q(anpcandidate:id()),
	?ASSERT(
		wf:q(rejected_comment) /= [],
		"Please specify a reason for skipping this paper."),
	anpcandidate:handle_reject_answerpaper(TId, CId);


%
% submit modal events
%
event({btn_submit_marks_box}) ->
	ep_osm_eval_v2_modals:modal_submit_paper();

event({btn_show_remaining}) ->
	itl:modal_close(),
	wf:wire("$('#navbar_page_no').dropdown('toggle')"),
	event({show, anpcandidate_answerpaper});


%
% show grievance modal event
%
event({show_grievance_modal})->
	ep_osm_eval_v2_modals:modal_student_grievance();


%
% show evaluator marking modal
%
event({show_evaluator_markings})->
	ep_osm_eval_v2_modals:modal_evaluator_markings();


%_______________________________________________________________________________
%
% route all other events to base modal.
%_______________________________________________________________________________

event(E) ->
	anpcandidate:event(E).

