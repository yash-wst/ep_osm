
-module(dig_ep_osm_exam_upload_to_result_processing_system).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("Result Upload To Result Processing System").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, _) -> false.



fids(display) -> [
	season_fk,
	faculty_code_fk,
	program_code_fk,
	subject_code_fk,
	anptestcourseid,
	teststatus,
	result_upload_status
];

fids(search) -> [
	season_fk,
	faculty_code_fk,
	program_code_fk,
	subject_code_fk,
	anptestcourseid
].

%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=fields:getfields(fids(search))
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("Result Upload To Result Processing System").



%------------------------------------------------------------------------------
% function - init
%------------------------------------------------------------------------------
init() ->
	ok.


%------------------------------------------------------------------------------
% function - fetch
%------------------------------------------------------------------------------

%..............................................................................
%
% []
%
%..............................................................................
fetch(D, _From, _Size, []) ->
	{
		D,
		[{custom, #panel {
			style="margin-top: 10px;",
			class="mycenter",
			body=layout:frame("Please select filters to search & upload.")
		}}]
	};


fetch(D, _From, _Size, Fs) ->

	%
	% init
	%
	FsFind = Fs ++ [
		% fields:build(teststatus, "completed"),
		% fields:build(result_upload_status, "")
	],


	%
	% get all docs matching search criteria
	%
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		anptests:getdb(), FsFind, 0, ?INFINITY
	),



	%
	% layout
	%
	FsDisplay = fields:getfields(fids(display)),
	Results = lists:map(fun(Doc) ->
		lists:map(fun(F) ->
			#dcell {val=itl:render(F)}
		end, itf:d2f(Doc, FsDisplay))
	end, Docs),


	Header = lists:map(fun(F) ->
		#dcell {type=header, val=F#field.label}
	end, FsDisplay),


	%
	% return
	%
	{
		D#dig {
			total=length(Docs),
			actions=[
				{upload_to_frp, "Upload Results", "Upload Results"}
			]
		},
		[Header] ++ Results
	}.


%------------------------------------------------------------------------------
% function - exports
%------------------------------------------------------------------------------
exports() -> [
].



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------
layout() ->
	dig:dig(?MODULE:get()).



%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
