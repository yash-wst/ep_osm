-module(ep_osm_eval_v2_page_nav_widget).
-compile(export_all).
-include("records.hrl").

get_page_navigation_widget(Filenames) ->
	Fs = anpcandidate:get_fs(),
	CanvasDataVal = fields:getuivalue(Fs, helper:l2a("anpcanvas_" ++ myauth:role())),
	Rows = layout_page_nos_rows(CanvasDataVal, Filenames),
	Elements =
	 [
		#panel{
			class="table-responsive-sm page-nav-widget-main hidden",
			body = [
				#table {
					html_id ="page_nav_widget1",
					class="table table-sm table-borderless m-0",
					rows=Rows
				}
			]
		}
	],

	wf:update("navbar-page-nav-widget", Elements).

layout_page_nos_rows(CanvasDataVal, List) ->
	layout_page_nos_rows(CanvasDataVal, List, 5).

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

layout_page_nos_cells(CanvasDataVal, {Index, AName}) ->
	Class = case lists:keyfind(AName, 1, CanvasDataVal) of
		false ->
			"btn-primary-outline";
		{AName, "{\"objects\":[]," ++ _} ->
			"btn-primary-outline";
		_ ->
			"btn-success page-nav-done-pages"
	end,
	#tablecell {body=[
		#link {
			class="align-bottom text-center page-nav-round-button btn btn-sm " ++ Class,
			url="#" ++ AName,
			text=Index,
			postback={page_nos, Index, AName}
		}
	]}.


filler_cells(N) ->
	lists:map(fun(_) -> #tablecell {body=""} end, lists:seq(1, N)).
