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
		class="dropdown-menu",
		style="z-index:2000;border: 1px solid #CFD1D7;border-radius: 12px;box-shadow: 0px 3px 6px #00000029;",
		body=#panel{
			class="table-responsive-sm page-nav-widget-main hidden",
			body = [
				#table {
					class="table table-sm table-borderless m-0",
					rows=Rows
				}
			]
		}
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
	Class = case lists:keyfind(AName, 1, CanvasDataVal) of
		false ->
			"btn-primary-outline";
		{AName, "{\"objects\":[]," ++ _} ->
			"btn-primary-outline";
		_ ->
			"btn-success page-nav-done-pages"
	end,

	#tablecell {body=get_page_no_button(Class, AName, Index)}.


filler_cells(N) ->
	lists:map(fun(_) -> #tablecell {body=""} end, lists:seq(1, N)).


get_page_no_button(Class, AName, Index) ->
	[
		#link {
			class="align-bottom text-center page-nav-round-button btn btn-sm " ++ Class,
			url="#" ++ AName,
			text=Index,
			postback={page_nos, Index, AName}
		}
	].
