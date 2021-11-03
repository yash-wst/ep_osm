-module(ep_osm_mscheme_handler).
-compile(export_all).
-include("records.hrl").
-include("records_ep_osm_mscheme.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({clicked, export_marker, WUId}) ->
	handle_clicked_export_marker(WUId);

event({insert_widget, undefined, {widget, _, _, _} = Widget}) ->
	handle_add_widget(?OSMMSC(Widget));

event({insert_widget, #field {} = F, {widget, _, _, _} = Widget}) ->
	handle_insert_widget(F, ?OSMMSC(Widget));

event(Other) ->
	?D(Other).




%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------

%..............................................................................
%
% handle - add widget
%
%..............................................................................

handle_add_widget(FWidget) ->
	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mscheme_api:get(Id),
	#field {subfields=Subfields} = FList = itf:d2f(Doc, ?OSMMSC({list_of_widgets, list_of_widgets})),


	%
	% build fs to save
	%
	FsToSave = [
		FList#field {subfields=Subfields ++ [FWidget]}
	],

	%
	% save
	%
	handle_save_and_reload(Id, FsToSave).

%..............................................................................
%
% handle - remove widget
%
%..............................................................................

handle_remove_widget(F) ->
	handle_insert_widget(F, []).



%..............................................................................
%
% handle - insert widget
%
%..............................................................................

handle_insert_widget(#field {baseid=FId}, FWidget) ->


	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mscheme_api:get(Id),
	FList = itf:d2f(Doc, ?OSMMSC({list_of_widgets, list_of_widgets})),


	%
	% build fs to save
	%
	FsToSave = [
		handle_replace_field(FList, FId, FWidget)
	],


	%
	% save
	%
	handle_save_and_reload(Id, FsToSave).




%..............................................................................
%
% handle replace field
%
%..............................................................................

handle_replace_field(#field {id=FId}, FId, FNew) ->
	FNew;
handle_replace_field(#field {subfields=undefined} = FTo, _FId, _FNew) ->
	FTo;
handle_replace_field(#field {subfields=Subfields} = FTo, FId, FNew) ->
	FTo#field {
		subfields=lists:foldl(fun(Fi, Acc) ->
			case handle_replace_field(Fi, FId, FNew) of
				[] -> Acc;
				F -> Acc ++ [F]
			end
		end, [], Subfields)
	}.



%..............................................................................
%
% handle save and reload
%
%..............................................................................

handle_save_and_reload(Id, FsToSave) ->

	case ep_osm_mscheme_api:save(FsToSave, ep_osm_mscheme:fs(all), Id) of
		{ok, _} ->
			helper:redirect(wf:uri());
		_ ->
			helper_ui:flash(error, "failed!", 5)
	end.


%..............................................................................
%
% handle clicked - export marker
%
%..............................................................................

handle_clicked_export_marker(WUId) when is_atom(WUId) ->
	handle_clicked_export_marker(?A2L(WUId));
handle_clicked_export_marker(WUId) ->

	%
	% init
	%
	Id = wf:q(id),
	{ok, Doc} = ep_osm_mscheme_api:get(Id),
	ExportMarkerList = itf:val(Doc, ?OSMMSC(list_of_export_markers)),
	FList = itf:d2f(Doc, ?OSMMSC({list_of_widgets, list_of_widgets})),

	%
	% FsSave
	%
	ExportMarkerList1 = lists:keydelete(WUId, 1, ExportMarkerList),
	ExportMarkerList2 = case wf:q(WUId) of
		undefined ->
			ExportMarkerList1;
		"on" ->
			ExportMarkerList1 ++ [{WUId, "on"}]
	end,


	%
	% save
	%
	FsToSave = [
		FList,
		itf:build(?OSMMSC(list_of_export_markers), ExportMarkerList2)
	],
	case ep_osm_mscheme_api:save(FsToSave, ep_osm_mscheme:fs(all), Id) of
		{ok, _} ->
			helper_ui:flash(success, get_msg(WUId, wf:q(WUId)), 5);
		_ ->
			helper_ui:flash(error, "failed! please reload page.")
	end.




%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% get message
%
get_msg(WUId, undefined) ->
	itx:format("~s, ~s", [
		ep_osm_mscheme_fields:get_question_id_from_wuid(?L2A(WUId)),
		"OFF"
	]);
get_msg(WUId, "on") ->
	itx:format("~s, ~s", [
		ep_osm_mscheme_fields:get_question_id_from_wuid(?L2A(WUId)),
		"ON"
	]).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
