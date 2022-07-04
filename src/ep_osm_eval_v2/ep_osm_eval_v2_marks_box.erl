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

		#panel{
			class="marks-box-header",
			body=[
				#panel {
					class="marks-box-icons marks-box-tick"
				},

				#span {
					id="anpcandidate_totalmarks",
					class="marks_box_text_bold",
					text = helper:f2s_v1( anpcandidate:get_total_marks(Fs)) ++ "/" ++ helper:state(testtotalmarks)
				},

				#panel {
					class="marks-box-icons marks-box-clock"
				},

				#span {
					html_id="time_spent",
					text="...",
					class="marks_box_text_bold"
				}
			]
		}
	],
	Elememt.

get_evaluated_percentage() ->
	[].

marks_box_eval_progress() ->
	[
		#panel {
			class="progress-container round-xlarge",
			body=[
				#panel {
					class="progressbar round-xlarge",
					style="width:25%",
					id="progress_percent_id"
				}
			]
		},

		#panel {
			text="40 % Evaluated",
			class="marks-box-text marks-box-progress-text",
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
			class="marks-box-container",
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

