-module(ep_osm_eval_v2_page_nav_widget).
-compile(export_all).
-include("records.hrl").

-define(NUMBER_OF_COLUMNS, 5).


%----------------------------------------------------------------------
%
% gets the page navigation widget
%
%----------------------------------------------------------------------
create_page_navigation_widget(Filenames) ->
	Fs = anpcandidate:get_fs(),
	CanvasDataVal = fields:getuivalue(Fs, helper:l2a("anpcanvas_" ++ myauth:role())),
	Rows = layout_page_nos_rows(CanvasDataVal, Filenames, ?NUMBER_OF_COLUMNS),

	TableContainer = #panel {
			class="table-responsive-sm",
			body = [
				#table {
					class="table table-sm table-borderless m-0",
					rows=Rows
				}
			]
	},

	wf:update("navbar-page-nav-widget", TableContainer).


%----------------------------------------------------------------------
%
% layout table rows
%
%----------------------------------------------------------------------
layout_page_nos_rows(CanvasDataVal, List, COLUMNS) ->

	{_, CellElements} = lists:foldl(fun(AName, {I, Cells}) ->
		{I+1, Cells ++ [layout_page_nos_cells(CanvasDataVal, {I, AName})]}
	end, {1, []}, List),

	ListOfCells = [lists:sublist(CellElements, X, COLUMNS) || X <- lists:seq(1, length(CellElements), COLUMNS)],
	lists:map(fun(Cs) ->
		Cs1 = case length(Cs) < COLUMNS of
			true -> Cs ++ filler_cells(COLUMNS - length(Cs));
			_ -> Cs
		end,
		#tablerow {cells=Cs1}
	end, ListOfCells).


%----------------------------------------------------------------------
%
% layout table cells
%
%----------------------------------------------------------------------
layout_page_nos_cells(CanvasDataVal, {Index, AName}) ->
	%
	% color button based on evaluator marking on that page
	%
	Class = case lists:keyfind(AName, 1, CanvasDataVal) of
		false ->
			"btn-outline-secondary"; % no canvas data yet
		{AName, "{\"objects\":[]," ++ _} ->
			"btn-outline-secondary"; % no canvas data yet
		_ ->
			"btn-success bg-success" % canvas data exists
	end,

	#tablecell {body=layout_page_no_button(Class, AName, Index)}.


filler_cells(N) ->
	lists:map(fun(_) -> #tablecell {body=""} end, lists:seq(1, N)).


layout_page_no_button(Class, AName, Index) ->
	[
		#link {
			style="height:30px;width:30px;",
			class="align-bottom text-center rounded-circle btn btn-sm " ++ Class,
			url="#" ++ AName,
			text=Index,
			postback={page_nos, Index, AName}
		}
	].
