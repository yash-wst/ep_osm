-module(ep_osm_eval_v2_toolbar).
-compile(export_all).
-include("records.hrl").


%-------------------------------------------------------------------------------
%
% layout toolbar button
%
%-------------------------------------------------------------------------------
layout_toolbar_button(Text, Bg_image_css_name, _Id) ->
	[
		#span {
			style="box-shadow:0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);",
			class="bg-white py-1",
			body=[

				%
				% clickable svg icon
				%
				#link {
					html_id =_Id,
					body=#panel {
						style="width:24px;height:24px;",
						class = io_lib:format("card-img-top my-0 mx-auto ~s ",
						 [Bg_image_css_name])
					}
				},

				%
				% text under icon
				%
				#panel {
					text=Text,
					style="font-size:8px;"
				}
			]
		}
	].



%-------------------------------------------------------------------------------
%
% layout toolbar buttons
%
%-------------------------------------------------------------------------------
layout_toolbar_buttons() ->
	[
		layout_toolbar_button("Rotate", "toolbar-icon-rotate", "toolbar_rotate"),
		layout_toolbar_button("Flip", "toolbar-icon-flip", "toolbar_flip"),
		layout_toolbar_button("Erase All", "toolbar-icon-eraseall", "toolbar_eraseall"),
		layout_toolbar_button("Undo","toolbar-icon-undo", "toolbar_undo")
	].



%-------------------------------------------------------------------------------
%
% layout toolbar top semicircle
%
%-------------------------------------------------------------------------------
layout_top_semicircle() ->
	#panel{
		style="height:15px;border-radius:150px 150px 0 0;background-color: white;
		box-shadow:0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
		border-top: 1px solid #CFD1D7;"
	}.


%-------------------------------------------------------------------------------
%
% layout toolbar bottom semicircle
%
%-------------------------------------------------------------------------------
layout_bottom_semicircle() ->
	#panel{
		style="height:15px;border-radius:0 0 150px 150px;background-color: white;
		box-shadow:0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);"
	}.



%-------------------------------------------------------------------------------
%
% layout toolbar
%
%-------------------------------------------------------------------------------
layout_toolbar() ->
	#span {
		html_id="toolbar_floating",
		style="z-index:1001;",
		class="bg-transparent float-end d-flex flex-column text-center
			position-fixed end-0 bottom-0 mb-4 me-4 p-0",
		body=[
			layout_top_semicircle(),

			layout_toolbar_buttons(),

			layout_bottom_semicircle()

		]
	}.


