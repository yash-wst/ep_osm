-module(ep_osm_eval_asserts).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% checks if all images are properly loaded on canvas
%------------------------------------------------------------------------------

check_all_images_are_loaded(CanvasData) ->
	{_, ImproperBgImagePageNos} = lists:foldl(fun({ImageName, JSONdata}, {PageNo, PageNos}) ->
		%
		% init
		%
		CurrentPageNo = PageNo + 1,
		{struct, Res} = mochijson2:decode(JSONdata),
		{struct, BgImageData} = proplists:get_value(
			<<"backgroundImage">>, Res, {struct, notFound}),

		SrcUrl = case BgImageData of
			notFound ->
				undefined;
			_ ->
				proplists:get_value(<<"src">>, BgImageData)
		end,

		case SrcUrl of
			undefined ->
				{CurrentPageNo, PageNos ++ [integer_to_list(CurrentPageNo)]};
			"" ->
				{CurrentPageNo, PageNos ++ [integer_to_list(CurrentPageNo)]};
			_ ->
				{CurrentPageNo, PageNos}
		end

	end, {0, []}, CanvasData),

	PageNoStr = string:join(ImproperBgImagePageNos, ", "),

	ErrorStr = itx:format(
		"All images are not properly loaded.
		Please press 'Erase All' for page numbers ~s and evaluate them.",
		[PageNoStr]
	),

	?ASSERT(
		length(ImproperBgImagePageNos) == 0,
		ErrorStr
	).



%------------------------------------------------------------------------------
% evaluator authorised
% we need to ensure that evaluator is assigned this paper and the assessment 
% state is valid
%------------------------------------------------------------------------------

evaluator_authorised(_TFs, Fs) ->

	%
	% init
	%
	ProfileId = myauth:profileid(),
	AnpState = itf:val(Fs, anpstate),
	ProfileIdForState = ep_osm_candidate:get_profileid_for_state(AnpState),
	EvalStats = [
		"anpstate_active",
		"anpstate_moderation",
		"anpstate_revaluation",
		"anpstate_moderation_reval"
	],


	?ASSERT(
		lists:member(AnpState, EvalStats),
		itx:format("Error! Document state: ~s", [?LN(?L2A(AnpState))])
	),


	?ASSERT(
		itf:val(Fs, ProfileIdForState) == ProfileId,
		"This document is not assigned to you!"
	).








%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
