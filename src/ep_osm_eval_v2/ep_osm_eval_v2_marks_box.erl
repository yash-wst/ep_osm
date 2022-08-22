-module(ep_osm_eval_v2_marks_box).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


marks_box_header(TFs, Fs) ->

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
					text = helper:f2s_v1( anpcandidate:get_total_marks(Fs)) ++ "/" ++ helper:state(testtotalmarks)
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

get_progress_bar() ->
	[
	"<div class='progress my-2'>
		<div class='progress-bar' role='progressbar' style='width: 25%;' aria-valuenow='25' aria-valuemin='0' aria-valuemax='100'>25%</div>
	</div>"
	].

marks_box_eval_progress() ->
	[
		get_progress_bar(),

		#panel {
			text="40 % Evaluated",
			class="text-center mb-2",
			id="marks_box_progress_id"
		}
	].

marking_scheme_layout(TFs, Fs) ->

	MarkingLayout = anpcandidate:layout_marking(TFs, Fs),

	[
		#panel {
			html_id="marks_box_mscheme",
			class="marks-box-mscheme hidden",
			body=[
				#panel {id=anpcandidate_marks, body=MarkingLayout}
			]
		}
	].

get_marks_box(TFs, Fs) ->
	[
		#panel {
			html_id="marks_box",
			style="z-index:4000;border-radius: 12px;box-shadow: 0px 3px 6px #00000029;",
			class="float-left position-fixed start-1 bottom-0 mb-2 bg-white p-3",
			body=[
				marks_box_header(TFs, Fs),

				marking_scheme_layout(TFs, Fs),

				marks_box_eval_progress(),

				#button {
					html_id="btn_submit_marks",
					text="Submit",
					class="marks-button-submit hidden",
					postback={show, anpcandidate_submit}
				}
			]
		}

	].

