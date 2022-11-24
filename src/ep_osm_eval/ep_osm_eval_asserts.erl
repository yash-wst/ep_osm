-module(ep_osm_eval_asserts).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



%
% checks if all images are properly loaded on canvas
%
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
