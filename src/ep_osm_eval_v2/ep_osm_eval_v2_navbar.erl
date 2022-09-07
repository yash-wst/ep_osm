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
		class="link-primary text-center mx-2 px-2",
		text=Text,
		actions=Action,
		url=Url,
		postback=Postback
	}.

layout_navbar_link_right(Text, Action) ->
	#link {
		class="link-primary text-center mx-2 px-2",
		text=Text,
		actions=Action
	}.


%..............................................................................
%
% layout - dropdown for page navigation widget
%
%..............................................................................
layout_page_number_table_dropdown() ->
	[
		#link {
			class="dropdown text-decoration-none",
			body=[
				#panel {
					html_id="navbar_page_no",
					text="",
					class="link-primary dropdown-toggle",
					data_fields=[
						{"bs-toggle", "dropdown"}
					]
				},
				#panel{
					class="dropdown-menu p-2 page-nav-dropdown shadow-lg border
					navbar-dropdown-offset",
					style="z-index:2000;border-radius:8px;overflow: auto;
					max-height:300px;",
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
% button to show evaluator markings
%
%..............................................................................
layout_link_evaluator_markings() ->
	case itxconfigs_cache:get2(show_evaluator_marking_to_moderator, true) of
		true ->
			case itxauth:role() of
				"anpmoderator" ->
					layout_navbar_link_right("Evaluator Marks",
						anpcandidate:actions(anpcandidate_evaluator_marking));
				_ -> []
			end;
		_ -> []
	end.


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
					#link{
						text="Show Grievance",
						class="link-primary text-center mx-2 px-2",
						postback= {show_grievance_modal}
					},

					layout_link_evaluator_markings(),

					layout_navbar_link_right("Question Paper",
						anpcandidate:actions(anpcandidate_questionpaper)),

					layout_navbar_link_right("Model Answers",
						anpcandidate:actions(anpcandidate_modelanswers)),

					#link {
						style="height:24px;width:24px;",
						class="d-inline-block mx-2 navbar-icon-help",
						actions=anpcandidate:actions(help)
					},

					#link {
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
