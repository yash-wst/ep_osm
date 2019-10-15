-module(ep_osm_menu).
-compile(export_all).
-include("records.hrl").


menu(?APPOSM_RECEIVER) ->
	[
		{?LN("OSM Exams"), [
			{dig_ep_osm_exam, ?VIEW, ?LN("Find Exams")}
		]}
	];


menu(_) ->
	[].
