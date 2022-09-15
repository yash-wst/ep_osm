-module(ep_osm_eval_v2_review_area).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



%..............................................................................
%
% layout - review panel
%
%..............................................................................
layout_panel(Class, Body) ->
	layout_panel( [], Class, Body).


layout_panel( [], Class, Body) ->
	#panel {
		class=Class,
		body=Body
	};

layout_panel(PanelID, Class, Body) ->
	#panel {
		id=PanelID,
		class=Class,
		body=Body
	}.

%..............................................................................
%
% layout - review view panels
%
%..............................................................................

layout_review_area(TFs, Fs) ->
	layout:grow(
		[
			layout_panel(
				anpcandidate_answerpaper,
				"anppanel visiblepanel col-sm-12",
				layout_answerpaper(TFs, Fs)
				),

			layout_panel(
				anpcandidate_questionpaper,
				"anppanel hidden col-sm-12 text-center",
				ep_osm_pdf:layout(TFs, questionpaper)
				),

			layout_panel(
				anpcandidate_modelanswers,
				"anppanel hidden col-sm-12 text-center",
				ep_osm_pdf:layout(TFs, modelanswers)
				),

			layout_panel(
				anpcandidate_remarks,
				"anppanel hidden col-sm-10 offset-sm-1 card",
				ep_osm_eval_v2_remarks:layout_remarks_panel(Fs)
				),

			layout_panel(
				anpcandidate_remaining_pages,
				"anppanel hidden col-sm-12 text-center
				bg-white",
				[]
				),

			layout_panel(
				anpcandidate_reject,
				"anppanel hidden col-sm-12",
				[]
				),

			layout_panel(
				anpcandidate_submit,
				"anppanel hidden col-sm-12",
				[]
				),

			layout_panel(
				help,
				"anppanel hidden col-sm-12",
				ep_osm_eval_v2_help:layout_help_panel(TFs, Fs)
				)
		]).




%..............................................................................
%
% layout - answer paper
%
%..............................................................................
layout_answerpaper(TFs, Fs) ->

	%
	% get image urls based on where the images are uploaded
	%

	ImgUrls = anpcandidate:get_image_urls(fields:getuivalue(TFs, aws_s3_dir), TFs, Fs),

	?ASSERT(length(ImgUrls) > 0, "Could not find scanned images for this candidate."),


	%
	% handle masking
	%
	ImgUrls1 = anpcandidate:get_image_urls_after_masking(ImgUrls, myauth:role()),


	%
	% get canvas data based on role
	%
	CanvasVal = fields:getuivalue(Fs, helper:l2a("anpcanvas_" ++ myauth:role())),


	%
	% get image elements and their file names
	%
	{Pages, ANames, _TotalPages} = lists:foldl(fun(ImgUrl, {AccPages, AccANames, AccPageNo}) ->
		%
		% get attachment name from url
		%
		AName = anpcandidate:get_aname_from_imgurl(ImgUrl),

		%
		% get canvasdata from from json which is indexed by attachment name
		%
		CanvasData = lists:keyfind(AName, 1, CanvasVal),

		%
		% Make Page Numbers
		%
		PageNo = AccPageNo+1,

		%
		% layout canvas page for this url
		%
		Page = layout_answerpaper_page(ImgUrl, AName, CanvasData, PageNo),

		%
		% accumulate pages and attachment name
		%
		{AccPages ++ [Page], AccANames ++ [AName], PageNo}

	end, {[], [], 0}, ImgUrls1),


	%
	% populate page navigation widget
	%
	ep_osm_eval_v2_page_nav_widget:create_page_navigation_widget(ANames),

	helper:state(filenames, ANames),


	%
	% layout all pages
	%
	[
		#panel {
			style="overflow:auto;",
			class="d-flex flex-column justify-content-center align-items-center
				all_pages",
			body=Pages
		}
	].




%..............................................................................
%
% layout - answer paper page
%
%..............................................................................
layout_answerpaper_page(ImgUrl=CanvasId, AName, CanvasData, PageNo) ->
	%
	%  create html tags
	%
	PageTitle = #panel{class="text-center", text=AName},

	CanvasTag = itx:format("<canvas id='~s' class='CanvasNum_~p'></canvas>",
		[CanvasId, PageNo]),

	CanvasBody = #panel{
		html_id=AName,
		style="width: 100%;overflow-x:auto;",
		class=itx:format("AnpPage PageNum_~p", [PageNo]), % used to border active page
		body= CanvasTag
		},

	%
	% extract canvas json
	%
	CanvasDataVal = case CanvasData of
		false -> "false";
		{AName, Data} -> Data
	end,

	%
	% call js function to init the canvas
	%
	JsFn = itx:format("ANP.layout_answerpaper_page(\"~s\", ~p);",
		[CanvasId, CanvasDataVal]),
	wf:wire(JsFn),

	%
	% layout each answer sheet image
	%
	layout_panel("w-100 bg-light",
		layout_panel("card",
			[
				layout_panel("card-header",
					[
						#h5 {
							class="card-title mb-0",
							body=PageTitle
						}
					]
				),

				layout_panel("card-body m-auto p-2", CanvasBody)
			]
		)
	).

%..............................................................................
%
% end
%
%..............................................................................
