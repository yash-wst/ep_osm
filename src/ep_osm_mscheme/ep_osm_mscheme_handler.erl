-module(ep_osm_mscheme_handler).
-compile(export_all).
-include("records.hrl").
-include("records_ep_osm_mscheme.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({insert_widget, undefined, #field {} = FWidget}) ->
	handle_add_widget(FWidget);

event({insert_widget, #field {} = F, #field {} = FWidget}) ->
	handle_insert_widget(F, FWidget);

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



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
