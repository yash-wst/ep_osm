-module(ep_osm_eval_v2_modals).
-compile(export_all).
-include("records.hrl").



%-------------------------------------------------------------------------------
%
% layout skip evaluation
%
%-------------------------------------------------------------------------------
layout_skip_evaluation_confirmation() ->
	[
		#span{
			text="Are you sure you want to skip evaluating this answer booklet?"
		},

		#br{},
		#br{},

		#span{
			class="d-flex",
			body=[
				ite:button(
					cancel_button,
					"Cancel",
					{close_skip_eval_modal},
					"btn btn-outline-primary"
				),
				ite:button(
					skip_button,
					"Yes, Skip",
					{reject_answerpaper, reject},
					"btn btn-primary"
				)
			]
		}
	].


%-------------------------------------------------------------------------------
%
% layout skip evaluation
%
%-------------------------------------------------------------------------------
layout_skip_evaluation_final() ->

	Ev = ite:build(confirm_reject, ?EDIT, helper:titlecase(helper:a2l(reject)), nobinding),
	Es = [
		#p {
			text="Please specify reasons for skipping evaluation of this paper",
			class="text-primary"
		}
	],
	Es1 = itl:instructions([
		{ok, "Answer paper pages not visible."},
		{ok, "Answer paper pages missing."},
		{ok, "Answer paper not properly scanned."}
	]),
	Es2 = [itl:get(?EDIT, [itf:textarea(?F(rejected_comment, "Comments"))], Ev#jevent{label="Skip Evaluation"}, oe2form)],

	[Es, Es1, Es2].



%-------------------------------------------------------------------------------
%
% layout submit pages remaining
%
%-------------------------------------------------------------------------------
layout_submit_pages_remaining() ->
	[
		#panel {
			class="d-flex flex-column",
			body=[
				#p{text=locale:get(anpcandidate_submit_pages_remaining_cannot) ++
					" " ++locale:get(anpcandidate_submit_pages_remaining_message)},
				#hr{},
				#span{
					body=ite:button(
						btn_show_rem_pages,
						locale:get(anpcandidate_submit_pages_remaining_ok),
						{btn_show_remaining},
						"mylabel btn btn-primary"
					)
				}
			]
		}
	].



%-------------------------------------------------------------------------------
%
% layout submit pages remaining
%
%-------------------------------------------------------------------------------
layout_submit() ->
	{LofDone, LofTodo} = count_canvas_marking_data(),

	case LofDone >= LofTodo of
		true ->
			{modal, anpcandidate:layout_submit_confirm()};
		false ->
			{modal, layout_submit_pages_remaining()}
	end.




%-------------------------------------------------------------------------------
%
% modal skip evaluation
%
%-------------------------------------------------------------------------------
modal_skip_evaluation_confirmation() ->
	Es = layout_skip_evaluation_confirmation(),
	itl:modal_fs(Es, medium, "Skip Evaluation").

modal_skip_evaluation_final() ->
	Es = layout_skip_evaluation_final(),
	?D("here"),
	itl:modal_fs(Es, large, "Skip Evaluation").




%-------------------------------------------------------------------------------
%
% modal submit paper
%
%-------------------------------------------------------------------------------
modal_submit_paper() ->
	{modal, Es} = layout_submit(),
	itl:modal_fs(Es,large, "Submit Confirmation").




%-------------------------------------------------------------------------------
%
% Misc
%
%-------------------------------------------------------------------------------
count_canvas_marking_data() ->
    Fs = anpcandidate:get_fs(),
	LofTodo = length(helper:state(filenames)),
	CanvasData = fields:getuivalue(Fs, helper:l2a("anpcanvas_" ++ anpcandidate:role())),
	LofDone = anpcandidate:get_marked_pages_count(CanvasData),

	{LofDone, LofTodo}.

