-module(ep_osm_eval_v2_modals).
-compile(export_all).
-include("records.hrl").



%-------------------------------------------------------------------------------
%
% modal skip evaluation
%
%-------------------------------------------------------------------------------
modal_skip_evaluation() ->
	Es = [
			#span{
				text="Are you sure you want to skip evaluating this answer booklet?"
			},

			#br{},
			#br{},

			#span{
				class="d-flex flex-row-reverse",
				body=[
					ite:button(
						skip_button,
						"Yes, Skip",
						{reject_answerpaper, reject},
						"btn btn-primary"
					),
					ite:button(
						cancel_button,
						"Cancel",
						{close_skip_eval_modal},
						"btn btn-outline-primary"
					)
				]
			}
		],

	itl:modal_fs(Es, medium, "Skip Evaluation").


%-------------------------------------------------------------------------------
%
% layout submit pages remaining
%
%-------------------------------------------------------------------------------
layout_submit_pages_remaining() ->
	[
		#panel {
			class="d-flex flex-column text-center",
			body=[
				#span {class="label label-warning", text=locale:get(anpcandidate_submit_pages_remaining)},
				#hr{},
				#p {text=locale:get(anpcandidate_submit_pages_remaining_cannot)},
				#p {text=locale:get(anpcandidate_submit_pages_remaining_message)},
				#hr{},
				#span{
					class="d-flex flex-row justify-content-center",
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
	Fs = anpcandidate:get_fs(),
	LofTodo = length(helper:state(filenames)),
	CanvasData = fields:getuivalue(Fs, helper:l2a("anpcanvas_" ++ anpcandidate:role())),
	LofDone = anpcandidate:get_marked_pages_count(CanvasData),

	case LofDone >= LofTodo of
		true ->
			{modal, anpcandidate:layout_submit_confirm()};
		false ->
			{modal, layout_submit_pages_remaining()}
	end.



%-------------------------------------------------------------------------------
%
% modal submit paper
%
%-------------------------------------------------------------------------------
modal_submit_paper() ->
	{modal, Es} = layout_submit(),
	itl:modal_fs(Es,medium, "").
