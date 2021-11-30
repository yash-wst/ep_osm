-module(ep_osm_mscheme_layout).
-compile(export_all).
-include("records.hrl").
-include("records_ep_osm_mscheme.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% layout - insert buttons
%------------------------------------------------------------------------------

insert_buttons(WUId, F) ->
	insert_buttons(WUId, F, ep_osm_mscheme_handler).

insert_buttons(WUId, F, Handler) ->
	[
		itl:get(?CREATE, [itf:textbox(?F(marks_per_question, "Marks Per Question"))], noevent, table),
		#hr {},


		%
		% question
		%
		insert_button(
			?WNAME_QUESTION, F,
			{widget, WUId, ?WTYPE_QUESTION, ?WTYPE_QUESTION}, Handler
		),
		#hr {},


		%
		% group
		%
		lists:map(fun(I) ->
			Label = io_lib:format("Group of ~p questions", [I]),
			insert_button(Label, F, {widget, WUId, ?WTYPE_GROUP, I}, Handler)
		end, lists:seq(2, 15)),
		#hr {},


		%
		% rule
		%
		lists:map(fun(I) ->
			#p {
				body=lists:map(fun(J) ->
					Label = io_lib:format("Any ~p of ~p", [I, J]),
					insert_button(Label, F, {widget, WUId, ?WTYPE_RULE, {I, J}}, Handler)
				end, lists:seq(I + 1, 12))
			}
		end, lists:seq(1, 10)),
		#hr {},


		%
		% other
		%
		insert_button("Or", F, {widget, WUId, ?WTYPE_RULE, ?WID_OR}, Handler)
	].



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

insert_button(Label, #field {} = F, {widget, _, _, _} = Widget, Handler) ->
	#button {
		style="margin: 5px; padding: 5px;",
		class="btn btn-sm btn-primary-outline",
		delegate=Handler,
		text=Label,
		postback={insert_widget, F, Widget}
	};

insert_button(Label, WUId, {widget, _, _, _} = Widget, Handler) ->
	#button {
		style="margin: 5px; padding: 5px;",
		class="btn btn-sm btn-primary-outline",
		delegate=Handler,
		text=Label,
		postback={insert_widget, WUId, Widget}
	}.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
