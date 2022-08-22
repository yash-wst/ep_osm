-module(ep_osm_eval_v2_navbar).
-compile(export_all).
-include("records.hrl").


get_navbar_link(Text, Action) ->
	get_navbar_link(Text, Action, "", noevent).

get_navbar_link(Text, Action, Url) ->
	get_navbar_link(Text, Action, Url, noevent).

get_navbar_link(Text, Action, Url, Postback) ->
	#link {
		class="link-secondary text-center mx-2 px-2",
		text=Text,
		actions=Action,
		url=Url,
		postback=Postback
	}.

get_page_number_table_dropdown() ->
	[
		#span {
			html_id="navbar_page_no",
			text="",
			class="link-secondary" % only to match color with links
		},

		#span {

				class="dropdown",
				body=[
					#panel {
						class="dropdown-toggle dropdown-toggle-split",
						data_fields=[
							{"bs-toggle", "dropdown"}
						]
					},
					#panel{
						class="dropdown-menu p-1",
						style="z-index:2000;border: 1px solid #CFD1D7;border-radius: 8px;box-shadow: 0px 3px 6px #00000029;overflow: auto;",
						% id='navbar-page-nav-widget-1',
						body =
						[
							#panel{
								id='navbar-page-nav-widget',
								body=[ "Hello"]
							}
						]
					}
				]
		}
	].

get_navbar_left_section() ->
	[
		#panel{
			class="d-flex align-items-center p-0",
			body=[
				get_navbar_link("Home", "", "/"),
				get_navbar_link("Answer Booklet", anpcandidate:actions(anpcandidate_answerpaper)),

				get_page_number_table_dropdown(),

				get_navbar_link("Remarks", anpcandidate:actions(anpcandidate_comments)),

				get_navbar_link("Skip Evaluation", "", [], {skip_eval_event})
			]
		}
	].



get_navbar_right_section() ->
	[
		#panel{
				class="d-flex justify-content-end align-items-center",
				body=[
					get_navbar_link("Question Paper",anpcandidate:actions(anpcandidate_questionpaper)),
					get_navbar_link("Model Answers", anpcandidate:actions(anpcandidate_modelanswers)),

					#span {
						style="height:24px;width:24px;",
						class="d-inline-block mx-2 navbar-icon-help",
						actions=anpcandidate:actions(help)
					},

					#span {
						style="height:24px;width:24px;",
						class="d-inline-block mx-2 navbar-icon-fullscreen",
						html_id="view-fullscreen"
					}
				]
			}
	].


%..............................................................................
%
% layout - navbar
%
%..............................................................................


layout_navbar() ->
	[
		#panel{
		class="sticky-top row g-0 p-2 border-top border-bottom border-secondary bg-white",
		body=[
			layout:g(6, get_navbar_left_section()),
			layout:g(6, get_navbar_right_section())
		]
	}].
