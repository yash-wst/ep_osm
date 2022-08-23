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
	event({submit, show_remaining});

event(E) ->
	anpcandidate:event(E).

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



%..............................................................................
%
% layout - review view panels
%
%..............................................................................

layout_review_area(TFs, Fs) ->
	#panel {
		class="d-flex justify-content-lg-center text-center",
		body=[
			#panel {
				class="anppanel visiblepanel",
				id=anpcandidate_answerpaper,
				body=layout_answerpaper(TFs, Fs)
			},
			#panel {
				class="anppanel",
				style="display:none;",
				id=anpcandidate_questionpaper,
				body=ep_osm_pdf:layout(TFs, questionpaper)
			},
			#panel {
				class="anppanel",
				style="display:none;",
				id=anpcandidate_modelanswers,
				body=ep_osm_pdf:layout(TFs, modelanswers)
			},
			#panel {
				class="anppanel m-auto",
				style="display:none;",
				id=anpcandidate_comments,
				body=anpcandidate:layout_comments(Fs)
			},
			#panel {
				class="anppanel",
				style="display:none;",
				id=anpcandidate_pages, body=[]
			},
			#panel {
				class="anppanel",
				style="display:none;",
				id=anpcandidate_reject,
				body=[]
			},
			#panel {
				class="anppanel",
				style="display:none;",
				id=anpcandidate_submit,
				body=[]
			},
			#panel {
				class="anppanel",
				style="display:none;",
				id=anpcandidate_evaluator_marking,
				body=anpcandidate:layout_evaluator_marking(TFs, Fs)
			},
			#panel {
				class="anppanel",
				style="display:none;",
				id=help,
				body=anpcandidate:layout_help(TFs, Fs)
			}
		]
	}.


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
	{Pages, ANames} = lists:foldl(fun(ImgUrl, {AccPages, AccANames}) ->
		%
		% get attachment name from url
		%
		AName = anpcandidate:get_aname_from_imgurl(ImgUrl),

		%
		% get canvasdata from from json which is indexed by attachment name
		%
		CanvasData = lists:keyfind(AName, 1, CanvasVal),

		%
		% layout canvas page for this url
		%
		Page = layout_answerpaper_page(ImgUrl, AName, CanvasData),


		%
		% accumulate pages and attachment name
		%
		{AccPages ++ [Page], AccANames ++ [AName]}

	end, {[], []}, ImgUrls1),


	%
	% populate page navigation widget
	%
	ep_osm_eval_v2_page_nav_widget:create_page_navigation_widget(ANames),

	%
	% populate page that shows remaining pages when submit
	%
	anpcandidate:layout_page_nos(ANames),
	helper:state(filenames, ANames),


	%
	% layout
	%
	[
		anpcandidate:layout_answerpaper_grievance(fields:getuivalue(Fs, anp_redressal_grievance)),
		#panel {
			class="all_pages", % To count no of pages in js
			body=Pages
		}
	].


%..............................................................................
%
% layout - answer paper page
%
%..............................................................................

layout_answerpaper_page(ImgUrl, AName, CanvasData) ->
	%
	%  create html tags
	%
	CanvasId = ImgUrl,
	CanvasTag = lists:flatten(io_lib:format("<canvas id='~s'></canvas>", [CanvasId])),

	%
	% create page anchor
	%
	AnchorTag = [
		lists:flatten(io_lib:format("
			<a id='~s' href='#'></a>", [AName]
		))
	],


	%
	% embed canvas and other html tags
	%
	Element = #panel {
				style="width: 100%; overflow-x: scroll;",
				body=[
					#panel {
						body=[AnchorTag]
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
	JsFn = lists:flatten(io_lib:format("ANP.layout_answerpaper_page(\"~s\", ~p);", [CanvasId, CanvasDataVal])),
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
