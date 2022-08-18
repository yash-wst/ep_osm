-module(ep_osm_eval_v2_toolbar).
-compile(export_all).
-include("records.hrl").

get_toolbar_card(Text, Bg_image_css_name, _Id) ->
	[
		#span {
			class="card p-2",
			body=[
				#link {
					body=#panel {
						style="width:24px;height:24px;",
						class = io_lib:format("card-img-top my-0 mx-auto ~s ", [Bg_image_css_name]) },
					html_id =_Id
				},

				#panel {text=Text, class="card-text"}
			]
		}
	].

toolbar_on_right() ->
	[
		#span {
			style="z-index:2000;",
			class="float-end d-flex position-fixed bottom-0 end-0 m-2",
			body=[
				get_toolbar_card("Rotate", "toolbar-icon-rotate", "toolbar_rotate"),
				get_toolbar_card("Flip", "toolbar-icon-flip", "toolbar_flip"),
				get_toolbar_card("Erase All", "toolbar-icon-eraseall", "toolbar_eraseall"),
				get_toolbar_card("Undo","toolbar-icon-undo", "toolbar_undo")
			]
		}
	].
