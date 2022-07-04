-module(ep_osm_eval_v2_toolbar).
-compile(export_all).
-include("records.hrl").


get_toolbar_icon(Text, Bg_image_css_name, _Id) ->
	[
		#span {
			class="toolbar-icon-container",
			body=[
				#link {
					body=#panel { class = io_lib:format("toolbar-icons ~s ", [Bg_image_css_name]) },
					html_id =_Id
				},

				#label {text=Text, class="toolbar-icon-text"}
			]
		}
	].

toolbar_on_right() ->
	[
		#span {
			class="toolbar-float-right",
			body=[
				get_toolbar_icon("Rotate", "toolbar-icon-rotate", "toolbar_rotate"),
				get_toolbar_icon("Flip", "toolbar-icon-flip", "toolbar_flip"),
				get_toolbar_icon("Erase All", "toolbar-icon-eraseall", "toolbar_eraseall"),
				get_toolbar_icon("Undo","toolbar-icon-undo", "toolbar_undo")
			]
		}
	].
