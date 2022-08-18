-module(ep_osm_eval_v2_navbar).
-compile(export_all).
-include("records.hrl").

get_navbar_left_section() ->
	[
		#panel{
			class="navbar-col",
			body=[
				#link {
					class="navbar-links navbar-links-left",
					text="Home",
					url="/"
				},

				#link {
					class="navbar-links navbar-links-left",
					text="Answer Booklet",
					actions=anpcandidate:actions(anpcandidate_answerpaper)
				},

				#span {
					html_id="page_no_display",
					text="",
					class="navbar-links navbar-links-left"
				},

				#link {
					body=#panel{
						style="height: 24px;width: 24px;",
						class="d-block my-0 mx-auto navbar-page-nav-expand"
						}
				},

				#link {
					class="navbar-links navbar-links-left",
					text="Remarks",
					actions=anpcandidate:actions(anpcandidate_comments)
				},

				#link {
					class="navbar-links navbar-links-left",
					text="Skip Evaluation",
					actions=anpcandidate:actions(anpcandidate_reject)
				}

			]
		}
	].

get_navbar_right_section() ->
	[
		#panel{
				class="navbar-col navbar-col-right",
				body=[
					#link {
						text="Question Paper",
						class="navbar-links navbar-links-right",
						actions=anpcandidate:actions(anpcandidate_questionpaper)
					},

					#link {
						text="Model Answers",
						class="navbar-links navbar-links-right",
						actions=anpcandidate:actions(anpcandidate_modelanswers)
					},

					#panel {
						class="navbar-icons navbar-icon-help",
						actions=anpcandidate:actions(help)
					},

					#panel {
						class="navbar-icons navbar-icon-fullscreen",
						html_id="view-fullscreen"
					}
				]
			}
	].

