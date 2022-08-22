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
					#button{
						text="Yes, Skip",
					 	class="skip-eval-buttons skip-eval-confirm",
					 	postback={reject_answerpaper, reject}
					 },

					#button{
						text="Cancel",
						class="skip-eval-buttons skip-eval-cancel",
						postback={close_skip_eval_modal}
					}
				]
			}
		],

	itl:modal_fs(Es, medium, "Skip Evaluation").
