-module(ep_osm_eval_v2_navbar).
-compile(export_all).
-include("records.hrl").

get_navbar_link(Text, Url, Action) ->
	#link {
		class="link-secondary text-center mx-2 px-2",
		text=Text,
		url=Url,
		actions=Action
	}.

get_page_number() ->
	[
		#span {
			html_id="page_no_display",
			text=""
		},
			#link {
				body=#panel{
					style="height:24px;width:24px;",
					class="d-block my-0 mx-auto navbar-page-nav-expand"
					}
			}
	].

get_navbar_left_section() ->
	[
		#panel{
			class="d-flex align-items-center p-0",
			body=[
				get_navbar_link("Home", "/",""),
				get_navbar_link("Answer Booklet", "",anpcandidate:actions(anpcandidate_answerpaper)),
				get_page_number(),

				get_navbar_link("Remarks", "",anpcandidate:actions(anpcandidate_comments)),

				get_navbar_link("Skip Evaluation", "",anpcandidate:actions(anpcandidate_reject))
			]
		}
	].

get_navbar_right_section() ->
	[
		#panel{
				class="d-flex justify-content-end align-items-center ms-auto me-1",
				body=[
					get_navbar_link("Question Paper", "",anpcandidate:actions(anpcandidate_questionpaper)),
					get_navbar_link("Model Answers", "",anpcandidate:actions(anpcandidate_modelanswers)),

					#panel {
						style="height:24px;width:24px;",
						class="mx-2 navbar-icon-help",
						actions=anpcandidate:actions(help)
					},

					#panel {
						style="height:24px;width:24px;",
						class="mx-2 navbar-icon-fullscreen",
						html_id="view-fullscreen"
					}
				]
			}
	].

