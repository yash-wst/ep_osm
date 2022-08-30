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
		#panel {
			class="bg-white p-2 shadow-lg",
			body=[

				%
				% clickable svg icon
				%
				#link {
					html_id =_Id,
					body=[
						#panel {
							style="width:25px;height:25px;transform:scale(1.4);",
							class = io_lib:format("card-img-top my-0 mx-auto ~s ",
							 [Bg_image_css_name])
						},

						%
						% clickable text under icon
						%
						#panel {
							text=Text,
							style="font-size:12px;"
						}
					]
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
		style="height:15px;border-radius:150px 150px 0 0;",
		class="shadow-lg bg-white"
	}.


%-------------------------------------------------------------------------------
%
% layout toolbar bottom semicircle
%
%-------------------------------------------------------------------------------
layout_bottom_semicircle() ->
	#panel{
		style="height:15px;border-radius:0 0 150px 150px;",
		class="shadow-lg bg-white"
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


