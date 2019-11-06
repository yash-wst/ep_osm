-module(ep_osm_menu).
-compile(export_all).
-include("records.hrl").


menu(?APPOSM_CONTROLLER) ->
	[
		{?LN("OSM Exams"), [
			{dig_ep_osm_exam_stats, ?VIEW, ?LN("Exam Statistics")}
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
