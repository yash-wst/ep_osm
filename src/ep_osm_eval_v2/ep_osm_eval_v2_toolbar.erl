-module(ep_osm_eval_v2_toolbar).
-compile(export_all).
-include("records.hrl").


%-------------------------------------------------------------------------------
%
% layout toolbar button
%
%-------------------------------------------------------------------------------
layout_toolbar_button2(Text, Fa_icon_name, _Id) ->
	[
		#panel {
			class="shadow-lg bg-white mb-0 rounded-0 px-0 py-1
			border border-dark border-top-0 border-bottom-0",
			body=[

				#link {
					html_id =_Id,
					body=[
						#panel {
							style="width:30px;height:30px;
							transform:scale(1.5);filter:grayscale(1);",
							class = "card-img-top shadow-0 mx-auto",
							body=itx:format("<i class='align-middle
								fas fa-fw fa-~s '></i>",
								[ Fa_icon_name ])
						},

						#panel {
							text=Text,
							style="font-size:12px;"
						}
					]
				}
			]
		}
	].


layout_toolbar_button(Text, Bg_image_css_name, _Id) ->
	ToolbarIcon = case Bg_image_css_name of
		"toolbar-icon-color" ->
			#panel{
				style="height: 25px;max-width: 25px;border-radius: 8px;background-color:red;",
				class="rounded-circle mx-auto swatchy-trigger
					swatchy-output swatchy-display"
				};
		_ ->
			#panel {
				style="width:25px;height:25px;transform:scale(1.4);",
				class=itx:format("card-img-top my-0 mx-auto ~s",
				 [Bg_image_css_name])
			}
	end,

	#panel {
		class="bg-white p-2 shadow-lg
		border border-dark border-top-0 border-bottom-0",
		body=[

			%
			% clickable svg icon
			%
			#link {
				html_id =_Id,
				body=[

					ToolbarIcon,

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
	}.



%-------------------------------------------------------------------------------
%
% layout toolbar buttons
%
%-------------------------------------------------------------------------------
layout_toolbar_buttons() ->
	[
		layout_toolbar_button("Color", "toolbar-icon-color", "toolbar_color"),
		layout_toolbar_button("Rotate", "toolbar-icon-rotate", "toolbar_rotate"),
		layout_toolbar_button("Flip", "toolbar-icon-flip", "toolbar_flip"),
		layout_toolbar_button("Erase All", "toolbar-icon-eraseall", "toolbar_eraseall"),
		layout_toolbar_button("Undo","toolbar-icon-undo", "toolbar_undo")
		% layout_toolbar_button2("Draw","pencil-alt", "toolbar_draw"),
		% layout_toolbar_button2("Add Text","italic", "toolbar_add_text")
	].



%-------------------------------------------------------------------------------
%
% layout toolbar top semicircle
%
%-------------------------------------------------------------------------------
layout_top_semicircle() ->
	#panel{
		style="height:15px;border-radius:150px 150px 0 0;",
		class="shadow-lg bg-white border border-dark border-bottom-0"
	}.


%-------------------------------------------------------------------------------
%
% layout toolbar bottom semicircle
%
%-------------------------------------------------------------------------------
layout_bottom_semicircle() ->
	#panel{
		style="height:15px;border-radius:0 0 150px 150px;",
		class="shadow-lg bg-white border border-dark border-top-0"
	}.



%-------------------------------------------------------------------------------
%
% layout toolbar
%
%-------------------------------------------------------------------------------
layout_toolbar(TestFs) ->
	layout_toolbar(TestFs, itf:val(TestFs, anptesttype)).

layout_toolbar(_, "anptesttype_thesis") ->
	[];

layout_toolbar(_, _) ->
	#span {
		html_id="toolbar_floating",
		style="z-index:1001;",
		class="bg-transparent float-end d-flex flex-column text-center
			position-fixed end-0 bottom-0 mb-2 me-2 p-0",
		body=[
			layout_top_semicircle(),

			layout_toolbar_buttons(),

			layout_bottom_semicircle()

		]
	}.


