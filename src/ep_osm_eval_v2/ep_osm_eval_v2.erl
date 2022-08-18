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

		%
		% layout nav bar
		%
		layout_navbar(),

    	marks_box(TFs, Fs),
    	toolbar(),
		#panel {
			class="review-area",
			body=layout:grow([
				layout_review_area(TFs, Fs)
			])
		}

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
% layout - navbar
%
%..............................................................................

layout_navbar() ->
	#panel{
		class="bg-white sticky-top row g-0 p-2 border-top border-bottom border-secondary ",
		body=[
			layout:g(6, ep_osm_eval_v2_navbar:get_navbar_left_section()),
			layout:g(6, ep_osm_eval_v2_navbar:get_navbar_right_section())
		]
	}.



%..............................................................................
%
% layout - review view panels
%
%..............................................................................

layout_review_area(TFs, Fs) ->
	#panel {
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
			class="anppanel remarks-section",
			style="display:none;",
			id=anpcandidate_comments,
			body=anpcandidate:layout_comments(Fs)
		},
		#panel {
			class="anppanel",
			style="display:none;",
			id=anpcandidate_reject,
			body=ep_osm_eval_v2_skip_eval:get_dialog_box()
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
	]}.


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
	% page numbers div
	%
	ep_osm_eval_v2_page_nav_widget:get_page_navigation_widget(ANames),

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


marks_box(TFs, Fs) ->
	ep_osm_eval_v2_marks_box:get_marks_box(TFs, Fs).

skip_eval() ->
	ep_osm_eval_v2_skip_eval:get_dialog_box().

toolbar() ->
	ep_osm_eval_v2_toolbar:toolbar_on_right().


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
