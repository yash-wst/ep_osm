-module(ep_osm_eval_v2_skip_eval).
-compile(export_all).
-include("records.hrl").

get_dialog_box() ->
	[
		#span {
			class="skip-eval-container-main",
			body=[
				#link {
					body=[#panel{class="skip-eval-close"}],
					postback={reject_answerpaper, no}
				},

				#span{text="Skip Evaluation", class="skip-eval-text-title"},

				#br{},

				#span{
					text="Are you sure you want to skip evaluating this answer booklet?",
					class="skip-eval-text-description"
				},

				#br{},
				#br{},

				#span{
					class="skip-eval-container-buttons",
					body=[
						#button{text="Yes, Skip", class="skip-eval-buttons skip-eval-confirm", postback={reject_answerpaper, reject}},
						#button{text="Cancel", class="skip-eval-buttons skip-eval-cancel", postback={reject_answerpaper, no}}
					]
				}
			]
		}
	].
