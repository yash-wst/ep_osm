-module(ep_osm_menu).
-compile(export_all).
-include("records.hrl").


menu(?APPOSM_CONTROLLER) ->
	[
		{?LN("OSM Exams"), [
			{dig_ep_osm_exam_stats, ?VIEW, ?LN("Exam Statistics")},
			{dig_ep_osm_exam_evaluation_stats, ?VIEW, ?LN("Exam Evaluation Statistics")},
			{dig_ep_osm_exam_evaluator_stats, ?VIEW, ?LN("Exam Evaluator Statistics")},
			{dig_ep_osm_bundle_stats, ?VIEW, ?LN("Scanning Statistics")}
		]}
	];


menu(?APPOSM_SCANUPLOADER) ->
	[
		{?LN("OSM Exams"), [
			{dig_ep_osm_exam_bundle, ?VIEW, ?LN("Find Exams")}
		]}
	];


menu(?APPOSM_RECEIVER) ->
	[
		{?LN("OSM Exams"), [
			{dig_ep_osm_exam_bundle, ?VIEW, ?LN("Find Exams")}
		]}
	];

menu(_) ->
	[].
