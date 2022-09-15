-module(ep_osm_eval_v2_help).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



%..............................................................................
%
% layout help panel
%
%..............................................................................

layout_help_panel(_TFs, _Fs) ->
	Instructions = itl:instructions([
		{ok, "On images mouse pointer acts as a red pen. Click and drag mouse to draw on images."},
		{ok, "If marking scheme (marks entry) panel is not visible, please move your mouse cursor to extreme left of the browser window."},
		{ok, "You can click on the buttons on top of the page to see different panels such as question paper, model answers, etc."},
		{ok, "For better experience please use Google Chrome"},
		{ok, "It is recommended to use Full Screen button for better view"}
	]),

	InstuctionsCard = #panel{
		class="card",
		body=Instructions
	},

	Elements = [
		InstuctionsCard
	],

	layout:g(8, 2, Elements).


