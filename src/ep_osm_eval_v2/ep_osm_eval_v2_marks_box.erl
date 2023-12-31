-module(ep_osm_eval_v2_marks_box).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%-------------------------------------------------------------------------------
%
% layout marks box header
%
%-------------------------------------------------------------------------------
layout_marks_box_header(TFs, Fs) ->

	MarkingLayout = anpcandidate:layout_marking(TFs, Fs),

	wf:wire(#api{name=timer_event, tag=f1}),

	Evaluation_time = fields:getuivalue(Fs,list_to_atom(myauth:role() ++ "_evaluation_time")),

	if
		Evaluation_time == [] ->
			wf:wire("WstTimer.start(0);");
		Evaluation_time > 0 ->
			wf:wire("WstTimer.start(" ++ Evaluation_time ++ ");");
		true ->
			wf:wire("WstTimer.start(0);")
	end,

	Elememt = [

		#span{
			class="d-flex flex-row justify-content-evenly align-items-end bd-highlight mb-2",
			body=[
				#span {
					style="width:24px;height:24px;",
					class="d-inline-block marks-box-tick"
				},

				#span {
					id="anpcandidate_totalmarks",
					style="color: #333333;",
					class="align-baseline mx-2",
					text = helper:f2s_v1( anpcandidate:get_total_marks(Fs))
					 ++ "/" ++ helper:state(testtotalmarks)
				},

				#span {
					style="width:24px;height:24px;",
					class="d-inline-block marks-box-clock"
				},

				#span {
					html_id="time_spent",
					text="...",
					class="align-middle mx-2"
				}
			]
		}
	],
	Elememt.



%-------------------------------------------------------------------------------
%
% layout marking scheme
%
%-------------------------------------------------------------------------------
layout_progress_bar() ->
	[
	"<div class='progress my-2' style='height: 10px;'>
		<div class='progress-bar' role='progressbar' style='width: 0%;'
			aria-valuenow='0' aria-valuemin='0' aria-valuemax='100'></div>
	</div>"
	].



%-------------------------------------------------------------------------------
%
% layout marking scheme
%
%-------------------------------------------------------------------------------
layout_marking_scheme(TFs, Fs) ->

	MarkingLayout = anpcandidate:layout_marking(TFs, Fs),

	[
		#panel {
			html_id="marks_box_mscheme",
			style="overflow:auto;max-height:550px;",
			class="hidden mb-2 me-n3 pe-2",
			body=[
				#panel {id=anpcandidate_marks, body=MarkingLayout}
			]
		}
	].



%-------------------------------------------------------------------------------
%
% layout marks box
%
%-------------------------------------------------------------------------------
layout_marks_box(TFs, Fs) ->

	Style = case itf:val(TFs, anptesttype) of
		"anptesttype_thesis" ->
			"display: none;";
		_ ->
			"width:250px;z-index:2001;border-radius: 12px;"
	end,

	[
		#panel {
			html_id="marks_box",
			style=Style,
			class="d-flex flex-column float-left position-fixed start-1 bottom-0
			 mb-2 ms-2 bg-white p-3 justify-content-center border border-dark shadow-lg",
			body=[
				layout_marking_scheme(TFs, Fs),

				layout_marks_box_header(TFs, Fs),

				layout_progress_bar()
			]
		}
	].


%-------------------------------------------------------------------------------
%
% Misc
%
%-------------------------------------------------------------------------------
update_progress_bar() ->
	%
	% get number of pages which have markings
	%
	{PagesMarked, TotalPages} = ep_osm_eval_v2_modals:count_canvas_marking_data(),
	Newprogress = PagesMarked * 100 div TotalPages,

	%
	% call js to update progress bar on UI
	%
	Script = itx:format("$('.progress-bar').attr('aria-valuenow', ~p).css('width', ~p+'%');", [Newprogress, Newprogress]),
	wf:wire(Script).
