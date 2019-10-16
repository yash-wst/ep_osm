-module (index_osmreceiver).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered.html"}).

title() ->
	?LN("OSM Receiver Dashboard").

heading() ->
	[].


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_RECEIVER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------

layout() ->
	"Welcome".



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------