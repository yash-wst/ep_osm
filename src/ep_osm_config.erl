-module(ep_osm_config).
-compile(export_all).
-include("records.hrl").


is_qc_enabled() ->
	itxconfigs_cache:get2(ep_osm_enable_qc, false).
