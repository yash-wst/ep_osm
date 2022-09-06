-module(ep_osm_eval_v2_page_nav_widget).
-compile(export_all).
-include("records.hrl").

%-------------------------------------------------------------------------------
%
% gets the page navigation widget
%
%-------------------------------------------------------------------------------
create_page_navigation_widget(Filenames) ->
	Fs = anpcandidate:get_fs(),
	CanvasDataVal = fields:getuivalue(Fs, helper:l2a("anpcanvas_" ++ myauth:role())),
	%
	% create a table with 5 columns
	%
	Rows = layout_page_nos_rows(dropdown, CanvasDataVal, Filenames, 5),

	TableContainer = #panel {
			class="table-responsive-sm",
			body = [
				#table {
					class="table table-borderless m-0",
					rows=Rows
				}
			]
	},

	wf:update("navbar-page-nav-widget", TableContainer).



%-------------------------------------------------------------------------------
%
% layout table rows
%
%------------------------------------------------------------------------------
layout_page_nos_rows(Location, CanvasDataVal, List, COLUMNS) ->
	%
	% create a list of all page number buttons
	%
	{_, CellElements} = lists:foldl(fun(AName, {I, Cells}) ->
		{I+1, Cells ++ [layout_page_nos_cells(Location, CanvasDataVal, {I, AName})]}
	end, {1, []}, List),

	%
	% split into sublists of N elements ~ ?COLUMNS
	%
	ListOfCells = [ lists:sublist(CellElements, X, COLUMNS)
		|| X <- lists:seq(1, length(CellElements), COLUMNS)
	],

	%
	% fill rows which have less than N elements with filler cells and emit rows
	%
	lists:map(fun(Cs) ->
		Cs1 = case length(Cs) < COLUMNS of
			true -> Cs ++ layout_filler_cells(COLUMNS - length(Cs));
			_ -> Cs
		end,
		#tablerow {cells=Cs1}
	end, ListOfCells).


%-------------------------------------------------------------------------------
%
% layout table cells
%
%-------------------------------------------------------------------------------

%
% creates round buttons for page navigation dropdown
%
layout_page_nos_cells(dropdown, CanvasDataVal, {Index, AName}) ->
	Class = get_button_class_based_on_canvas_data(AName, CanvasDataVal),

	#tablecell {
		body=#button{
			style="height:33px;width:33px;",
			class="rounded-circle btn btn-sm " ++ Class,
			text=Index,
			postback={page_nav_dropdown,{page_nos, Index, AName}}
		}};

%
% creates square buttons for remaining pages panel
%
layout_page_nos_cells(pages_panel,CanvasDataVal, {Index, AName}) ->
	Class = get_button_class_based_on_canvas_data(AName, CanvasDataVal),

	#tablecell {
		body=#span{
			class="card mb-0",
			body=#button{
				% style="height:33px;width:100%;",
				class="w-100 btn " ++ Class,
				text=Index,
				postback={page_nos, Index, AName}
			}
		}
	}.

%
% filler cells to maintain table column number
%
layout_filler_cells(N) ->
	lists:map(fun(_) -> #tablecell {body=""} end, lists:seq(1, N)).


%----------------------------------------------------------------------
%
% misc
%
%----------------------------------------------------------------------

%
% gets button color based on canvas markings data
%
get_button_class_based_on_canvas_data(AName, CanvasDataVal) ->
	%
	% color button based on evaluator marking on that page
	%
	Class = case lists:keyfind(AName, 1, CanvasDataVal) of
		false ->
			"btn-outline-secondary"; % no canvas data yet
		{AName, "{\"objects\":[]," ++ _} ->
			"btn-outline-secondary"; % no canvas data yet
		_ ->
			"btn-success" % canvas data exists
	end,
	Class.

