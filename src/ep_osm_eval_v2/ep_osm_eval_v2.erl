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
	ep_osm_eval_v2_modals:modal_skip_evaluation_confirmation();

event({close_skip_eval_modal}) ->
	itl:modal_close(),
	event({reject_answerpaper, no});

event({reject_answerpaper, reject}) ->
	ep_osm_eval_v2_modals:modal_skip_evaluation_final();

event(confirm_reject) ->
	TId = wf:q(anptest:id()),
	CId = wf:q(anpcandidate:id()),
	?ASSERT(
		wf:q(rejected_comment) /= [],
		"Please specify a reason for skipping this paper."),
	anpcandidate:handle_reject_answerpaper(TId, CId);


event({btn_submit_marks_box}) ->
	ep_osm_eval_v2_modals:modal_submit_paper();

event({btn_show_remaining}) ->
	itl:modal_close(),
	wf:wire("$('#navbar_page_no').dropdown('toggle')"),
	event({show, anpcandidate_answerpaper});

event({show_grievance_modal})->
	ep_osm_eval_v2_modals:modal_student_grievance();

event({show_evaluator_markings})->
	ep_osm_eval_v2_modals:modal_evaluator_markings();

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
% layout - review panel
%
%..............................................................................
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

	% anchor tag for scrolling
	AnchorTagWitPageImage = #link{
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
	JsFn = itx:format("ANP.layout_answerpaper_page(\"~s\", ~p);", [CanvasId, CanvasDataVal]),
	wf:wire(JsFn),

   	% akit_card:layout(PageTitle, AnchorTagWitPageImage).
	#panel {
		class="w-100 bg-light",
		body=[
			#panel {
				class="card",
				body=[
					#panel {
						class="card-header",
						body=#h5 {
							class="card-title mb-0",
							body=PageTitle
						}
					},
					#panel {
						class="card-body m-auto",
						body=AnchorTagWitPageImage
					}
				]
			}
		]
	}.



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
