-module(ep_osm_helper).
-compile(export_all).
-include("records.hrl").
-include_lib("itx/include/records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%------------------------------------------------------------------------------
% start
%------------------------------------------------------------------------------

completed_state_of("anpevaluator") ->
	"anpstate_completed";
completed_state_of("anpmoderator") ->
	"anpstate_moderation_completed";
completed_state_of("anprevaluator") ->
	"anpstate_revaluation_completed";
completed_state_of("anpmoderator_reval") ->
	"anpstate_moderation_reval_completed";
completed_state_of(Role) ->
	throw(Role).


get_anpstate_shorthand(State) ->
	?LN(?L2A(State++"_min")).



%..............................................................................
%
% get canvases without background image
%
%..............................................................................

get_canvases_without_background_image(CanvasData) ->
	lists:filter(fun({_ImageName, JSONdata}) ->
		%
		% init
		%
		{struct, Res} = mochijson2:decode(JSONdata),
		{struct, BgImageData} = proplists:get_value(
			<<"backgroundImage">>, Res, {struct, notFound}
		),


		SrcUrl = case BgImageData of
			notFound ->
				undefined;
			_ ->
				proplists:get_value(<<"src">>, BgImageData)
		end,

		case SrcUrl of
			undefined ->
				true;
			"" ->
				true;
			_ ->
				false
		end
	
	end, CanvasData).



getcount_canvases_without_background_image(CanvasData) ->
	length(get_canvases_without_background_image(CanvasData)).

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
