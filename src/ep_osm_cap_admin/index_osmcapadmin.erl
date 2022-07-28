-module (index_osmcapadmin).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("OSM CAP Centre Admin Dashboard").

heading() ->
	[].


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_CAPADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------

layout() ->
	helper:redirect("/dig_ep_osm_exam_evaluation_stats").



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
