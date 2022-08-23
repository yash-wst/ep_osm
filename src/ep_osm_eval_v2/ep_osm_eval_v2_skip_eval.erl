-module(ep_osm_eval_v2_skip_eval).
-compile(export_all).
-include("records.hrl").

get_dialog_box() ->
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
