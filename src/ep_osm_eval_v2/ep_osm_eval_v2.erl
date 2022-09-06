-module(ep_osm_eval_v2).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


pagejs() -> [
	fabricjs,
	"/lib/ep_osm/priv/static/js/ep_osm_eval_v2.js"
].

pagecss() -> [
	"/lib/ep_osm/priv/static/css/ep_osm_eval_v2.css"
].


main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/adminkit/html/entered.html"}).

title() ->
	?LN("Evaluation").

heading() ->
	?LN("Evaluation").

%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------

access(_, ?APPOSM_EVALUATOR) -> true;
access(_, ?APPOSM_MODERATOR) -> true;
access(_, ?APPOSM_REVALUATOR) -> true;
access(_, ?APPOSM_MODERATOR_REVAL) -> true;
access(_, _) -> false.


%------------------------------------------------------------------------------
% event
%------------------------------------------------------------------------------
event(noevent) ->
	[];

event({skip_eval_event}) ->
	ep_osm_eval_v2_modals:modal_skip_evaluation();

event({close_skip_eval_modal}) ->
	itl:modal_close(),
	event({reject_answerpaper, no});

event({btn_submit_marks_box}) ->
	ep_osm_eval_v2_modals:modal_submit_paper();

event({btn_show_remaining}) ->
	itl:modal_close(),
	wf:wire("$('#navbar_page_no').dropdown('toggle')"),
	event({show, anpcandidate_answerpaper});

event({add_remark}) ->
	Res = anpcandidate:addcomment(
			wf:q(anptest:id()),
			wf:q(anpcandidate:id()),
			anpcandidate:get_comment_key(),
			wf:q(txtarea_remarks_id)
		),
	ep_osm_eval_v2_remarks:layout_remarks_update(helper_api:doc2fields(Res));

event({page_nav_dropdown, {page_nos, Index, AName}})->
	wf:wire("$('#navbar_page_no').dropdown('toggle')"),
	wf:redirect("#" ++ AName),
	anpcandidate:event({page_nos, Index, AName});

event(E) ->
	anpcandidate:event(E).



%---------------------------------------------------------------------------------------------------
% API EVENTS
%---------------------------------------------------------------------------------------------------
api_event(canvas_save, canvas_save, [CanvasId, CanvasData]) ->
	Res = anpcandidate:api_event(canvas_save, canvas_save, [CanvasId, CanvasData]),
	ep_osm_eval_v2_marks_box:update_progress_bar(),
	Res;

api_event(X, Y, Z) ->
	anpcandidate:api_event(X, Y, Z).



%------------------------------------------------------------------------------
% layout
%------------------------------------------------------------------------------
layout() ->

	%
	% init data
	%
	TId = wf:q(anptest:id()),
	TFs = anptests:get(TId),
	Fs = anpcandidates:get(anpcandidate:db(), wf:q(anpcandidate:id())),

	%
	% init layout
	%
	layout_init_page(),



	%
	% layout elements
	%
	Elements = [
		layout_navbar(),

    	layout_marks_box(TFs, Fs),

    	layout_toolbar(),

    	layout_review_area(TFs, Fs)
	],

	wf:wire(#api {name=canvas_save, tag=canvas_save}),
	wf:wire(#api {name=calculate_pages_done, tag=calculate_pages_done}),

	%
	% update progress bar on initial page load
	%
	ep_osm_eval_v2_marks_box:update_progress_bar(),

	Elements.



%..............................................................................
%
% layout - init page
%
%..............................................................................

layout_init_page() ->
	%
	% hide sidebar
	%
	akit_sidebar:collapse().

layout_panel(PanelID, Body, Class) ->
	#panel {
		id=PanelID,
		body=Body,
		class=Class
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
				layout_answerpaper(TFs, Fs),
				"anppanel visiblepanel col-sm-12"
				),

			layout_panel(
				anpcandidate_questionpaper,
				ep_osm_pdf:layout(TFs, questionpaper),
				"anppanel hidden col-sm-12 text-center"
				),

			layout_panel(
				anpcandidate_modelanswers,
				ep_osm_pdf:layout(TFs, modelanswers),
				"anppanel hidden col-sm-12 text-center"
				),

			layout_panel(
				anpcandidate_remarks,
				ep_osm_eval_v2_remarks:layout_remarks_panel(Fs),
				"anppanel hidden col-sm-10 offset-sm-1 card"
				),

			layout_panel(
				anpcandidate_remaining_pages,
				[],
				"anppanel hidden col-sm-12 text-center
				bg-white"
				),

			layout_panel(
				anpcandidate_reject,
				[],
				"anppanel hidden col-sm-12"
				),

			layout_panel(
				anpcandidate_submit,
				[],
				"anppanel hidden col-sm-12"
				),

			layout_panel(
				help,
				anpcandidate:layout_help(TFs, Fs),
				"anppanel hidden col-sm-12"
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
	% layout
	%
	[
		#panel {
			style="overflow:auto;",
			class="d-flex flex-column justify-content-center align-items-center",
			body=[
				anpcandidate:layout_answerpaper_grievance(
					fields:getuivalue(Fs, anp_redressal_grievance)),

				#panel {
					body=Pages
				}
			]
		}
	].




%..............................................................................
%
% layout - answer paper page
%
%..............................................................................
layout_answerpaper_page(ImgUrl, AName, CanvasData, PageNo) ->
	%
	%  create html tags
	%
	CanvasId = ImgUrl,
	CanvasTag = itx:format("<canvas id='~s' class='CanvasNum_~p'></canvas>",
		[CanvasId, PageNo]),


	%
	% create page anchor
	%
	AnchorTag = [
		itx:format("<a id='~s' href='#'></a>", [AName])
	],


	%
	% embed canvas and other html tags
	%
	Element = #panel {
				style="width: 100%; overflow-x: scroll;",
				class="text-center layout-answer-paper-page",
				html_id=itx:format("PageNum_~p", [PageNo]),
				body=[
					#panel {
						%
						% answer paper image name and anchor tag for scrolling
						%
						class="bg-light text-muted mb-1",
						body=[AName, AnchorTag]
					},
					CanvasTag
				]
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
	JsFn = itx:format("ANP.layout_answerpaper_page(\"~s\", ~p);", [CanvasId, CanvasDataVal]),
	wf:wire(JsFn),

	Element.




%------------------------------------------------------------------------------
% misc UI elements
%------------------------------------------------------------------------------
layout_navbar() ->
	ep_osm_eval_v2_navbar:layout_navbar().

layout_marks_box(TFs, Fs) ->
	ep_osm_eval_v2_marks_box:layout_marks_box(TFs, Fs).

layout_toolbar() ->
	ep_osm_eval_v2_toolbar:layout_toolbar().


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
