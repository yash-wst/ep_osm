-module(ep_osm_eval_v2_navbar).
-compile(export_all).
-include("records.hrl").


%..............................................................................
%
% layout navbar links
%
%..............................................................................
layout_navbar_link_left(Text, Action, Url, Postback) ->
	#link {
		class="link-secondary text-center mx-2 px-2",
		text=Text,
		actions=Action,
		url=Url,
		postback=Postback
	}.

layout_navbar_link_right(Text, Action, Url, Postback) ->
	#link {
		class="link-primary text-center mx-2 px-2",
		text=Text,
		actions=Action,
		url=Url,
		postback=Postback
	}.


%..............................................................................
%
% layout - dropdown for page navigation widget
%
%..............................................................................
layout_page_number_table_dropdown() ->
	[
		#span {
			class="dropdown",
			body=[
				#panel {
					html_id="navbar_page_no",
					text="",
					class="link-secondary dropdown-toggle",
					data_fields=[
						{"bs-toggle", "dropdown"}
					]
				},
				#panel{
					class="dropdown-menu p-2 page-nav-dropdown shadow-lg border
					navbar-dropdown-offset",
					style="z-index:2000;border-radius:8px;overflow: auto;",
					body =
					[
						#panel{
							id='navbar-page-nav-widget',
							body=[]
						}
					]
				}
			]
		}
	].


%..............................................................................
%
% layout - left side of navbar
%
%..............................................................................
layout_navbar_left_section() ->
	[
		#panel{
			class="d-flex align-items-center p-0",
			body=[
				layout_navbar_link_left("Home", "", "/", noevent),
				layout_navbar_link_left("Answer Booklet",
					anpcandidate:actions(anpcandidate_answerpaper), "", noevent),

				layout_page_number_table_dropdown(),

				layout_navbar_link_left("Remarks",
					anpcandidate:actions(anpcandidate_remarks), "", noevent),

				layout_navbar_link_left("Skip Evaluation", "", [], {skip_eval_event}),
				layout_navbar_link_left("Submit", "", [], {btn_submit_marks_box})
			]
		}
	].



%..............................................................................
%
% layout - right side of navbar
%
%..............................................................................
layout_navbar_right_section() ->
	[
		#panel{
				class="d-flex justify-content-end align-items-center",
				body=[
					layout_navbar_link_right("Question Paper",
						anpcandidate:actions(anpcandidate_questionpaper), "", noevent),

					layout_navbar_link_right("Model Answers",
						anpcandidate:actions(anpcandidate_modelanswers), "", noevent),

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
			layout:g(6, layout_navbar_left_section()),
			layout:g(6, layout_navbar_right_section())
		]
	}].
