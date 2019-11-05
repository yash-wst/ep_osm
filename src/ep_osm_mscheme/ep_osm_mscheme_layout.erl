-module(ep_osm_mscheme_layout).
-compile(export_all).
-include("records.hrl").
-include("records_ep_osm_mscheme.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% layout - insert buttons
%------------------------------------------------------------------------------

insert_buttons(WUId, F) ->
	[

		%
		% question
		%
		insert_button(?WNAME_QUESTION, F, ?OSMMSC({widget, WUId, ?WTYPE_QUESTION, ?WTYPE_QUESTION})),
		#hr {},


		%
		% group
		%
		lists:map(fun(I) ->
			Label = io_lib:format("Group of ~p questions", [I]),
			insert_button(Label, F, ?OSMMSC({widget, WUId, ?WTYPE_GROUP, I}))
		end, lists:seq(2, 5)),
		#hr {},


		%
		% rule
		%
		lists:map(fun(I) ->
			lists:map(fun(J) ->
				Label = io_lib:format("Any ~p of ~p", [I, J]),
				insert_button(Label, F, ?OSMMSC({widget, WUId, ?WTYPE_RULE, {I, J}}))
			end, lists:seq(I + 1, 5))
		end, lists:seq(1, 5))
	].



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

insert_button(Label, #field {} = F, #field {} = FWidget) ->
	#button {
		style="margin: 10px; padding: 10px;",
		class="btn btn-sm btn-primary-outline",
		delegate=ep_osm_mscheme_handler,
		text=Label,
		postback={insert_widget, F, FWidget}
	}.



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
