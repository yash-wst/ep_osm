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

event({add_remark}) ->
	Res = anpcandidate:addcomment(
			wf:q(anptest:id()),
			wf:q(anpcandidate:id()),
			anpcandidate:get_comment_key(),
			wf:q(txtarea_remarks_id)
		),
	layout_comments_update(helper_api:doc2fields(Res));

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
				anpcandidate_comments,
				layout_comments_panel(Fs),
				"anppanel hidden col-sm-10 offset-sm-1 card"
				),

			layout_panel(
				anpcandidate_pages,
				[],
				"anppanel hidden offset-sm-2 col-sm-8 offset-sm-2 text-center"
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
				class="text-center layout-answer-paper-page",
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
	JsFn = lists:flatten(io_lib:format("ANP.layout_answerpaper_page(\"~s\", ~p);", [CanvasId, CanvasDataVal])),
	wf:wire(JsFn),

	Element.



%-----------------------------------------------------------------------------------------------
%
% PANEL - COMMENTS
%
%-----------------------------------------------------------------------------------------------
layout_comments_panel(Fs) ->
	#panel{
		class="text-start p-5",
		body=[

		#textarea {
			id=txtarea_remarks_id,
			style="border:none;",
			class="form-control",
			placeholder="Write your remarks here...",
			text=""
		},

		#hr{
			 style="height:1px;",
			 class="mx-0 my-3"
		},

		ite:button(
			btn_add_remarks,
			"Add Remark",
			{add_remark},
			"btn btn-primary"
		),

		#hr{},

		#table {
			class="table table-bordered table-hover table-sm",
			%
			% layout all comment rows
			%
			rows= lists:map(fun({K, V}) ->
				[Date, Time, IP, User] = case string:tokens(K, " ") of
					[D,T,I,U] -> [D,T,I,U];
					[D,T,I,U,U1] -> [D,T,I,U++" "++U1] % patch - space in username
				end,
				#tablerow {cells=[layout_comment(Date, Time, IP, User, V)]}
			end, lists:reverse(fields:getuivalue(Fs, comments)))
		}
		]
	}.




%------------------------------------------------------------------------------
%
% layout comment
%
%------------------------------------------------------------------------------
layout_comment(Date, Time, IP, Username, Message) ->
	#panel{
		body=[
			#hr{
			 style="height:1px;border:none;margin:0;",
			 class="mt-3"
			},

			#panel{
				style="font-size:12px",
				class="text-secondary fw-light mt-3",
				text=itx:format("~ts   ~ts   ~ts   ~ts", [Date, Time, IP, Username])
			},
			#panel{
				class="text-dark mt-1",
				body=Message
			}
		]
	}.

layout_comments_update(Fs) ->
	wf:update(anpcandidate_comments, layout_comments_panel(Fs)).


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
