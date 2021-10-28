-module(ep_osm_dashboard).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%------------------------------------------------------------------------------
% layout
%------------------------------------------------------------------------------

layout() ->
	layout(configs:get(ep_osm, false)).

layout(true) ->
	Key = {?MODULE, layout},
	layout:grow([
		widget_evaluation_stats(),
		itxdoc_cache:get(Key, fun layout_from_db/0, 10)
	]);
layout(false) ->
	[].


layout_from_db() ->
	[
		widget_scanning_stats(),
		widget_active_test_count(),
		widget_scheduled_test_count(),
		widget_reminders_sent_today()
	].


%------------------------------------------------------------------------------
% widgets
%------------------------------------------------------------------------------

widget_active_test_count() ->
	Count = anptests:getcount_by_teststatus(?ACTIVE),
	akit_card:layout_widget(
		"Active tests", Count, "info", "# of active tests" 
	).


widget_scheduled_test_count() ->
	Count = anptests:getcount_by_teststatus(?SCHEDULED),
	akit_card:layout_widget(
		"Scheduled tests", Count, "info", "# of tests scheduled" 
	).


widget_reminders_sent_today() ->
	Count = dig_ep_osm_exam_evaluation_stats:number_of_reminders_sent_today(),
	akit_card:layout_widget(
		"Reminders", Count, "info", "# of reminders sent today" 
	).


widget_scanning_stats() ->

	Stats = ep_osm_bundle_api:get_stats_of_active_seasons(),
	
	Es = [
		akit_card:layout_widget(
			"Inwarded",
			proplists:get_value("inward_completed", Stats, 0),
			"info",
			"# of booklets",
			[
				{border_class, "border border-primary"}
			]
		),
		akit_card:layout_widget(
			"Scanned",
			proplists:get_value("scanning_completed", Stats, 0),
			"info",
			"# of booklets",
			[
				{border_class, "border border-primary"}
			]
		),
		akit_card:layout_widget(
			"Uploaded",
			proplists:get_value("upload_completed", Stats, 0),
			"info",
			"# of booklets",
			[
				{border_class, "border border-success"}
			]
		),
		akit_card:layout_widget(
			"Total",
			proplists:get_value("inward_completed", Stats, 0) +
			proplists:get_value("scanning_completed", Stats, 0) +
			proplists:get_value("upload_completed", Stats, 0),
			"info",
			"# of booklets",
			[
				{border_class, "border border-secondary"}
			]
		)
	],

	akit_card:layout(
		#panel {body=[
			akit_icon:get(barcode, [{size, "fa-2x"}]),
			#span {text="Scanning Status"}
		]},
		layout:grow(Es)
	).



widget_evaluation_stats() ->

	%
	% get stats
	%
	Key = {?MODULE, widget_evaluation_stats},
	Stats = itxdoc_cache:get(Key, fun() ->
		ep_osm_exam_api:get_stats_consolidated_evaluation_count_of_active_tests()
	end, 10),


	%
	% get charts
	%
	Es1 = widget_evaluation_stats("Evaluation Status", Stats, [
		anpstate_yettostart,
		anpstate_active,
		anpstate_completed,
		anpstate_evaluation_rejected
	]),
	Es2 = widget_evaluation_stats("Moderation Status", Stats, [
		anpstate_moderation,
		anpstate_moderation_completed
	]),
	Es3 = widget_evaluation_stats("Revaluation Status", Stats, [
		anpstate_revaluation,
		anpstate_revaluation_completed
	]),


	%
	% return widget
	%
	layout:grow(lists:map(fun(Es) ->
		layout:g(4, Es)
	end, [Es1, Es2, Es3])).


widget_evaluation_stats(Title, Stats, States) ->

	Data = lists:map(fun(State) ->
		{?LN(State), proplists:get_value(?A2L(State), Stats, 0)}
	end, States),
	Es = akit_chart:doughnut(helper:uidintstr(), Data),
	akit_card:layout(Title, Es).




%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
