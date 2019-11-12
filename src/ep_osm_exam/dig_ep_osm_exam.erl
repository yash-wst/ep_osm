
-module(dig_ep_osm_exam).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Exams").

heading() ->
	title().

form() ->
	ep_osm_exam.


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=anptest:fs(search),
		size=25
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exams").



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
fetch(D, From, Size, Fs) ->

	%
	% fetch documents from db
	%
	Rec = db2_find:getrecord_by_fs(anptests:getdb(), Fs, From, Size),
	#db2_find_response {docs=Docs}  = db2_find:find(
		Rec#db2_find {sort=anptest:fs(search)}
	),

	%
	% layout results
	%
	Results = lists:map(fun(Doc) ->

		%
		% layout cells
		%
		FsDoc = itf:d2f(Doc, anptest:fs(search)),
		FsIndex = itf:d2f(Doc, anptest:fs(index)),
 		[
			#dcell {val=helper_ui:layout_slinks(anptest, FsIndex)}

		] ++ lists:map(fun(F) ->
			#dcell {val=itl:render(F)}
		end, FsDoc) ++ [
			#dcell {val=layout_files(Doc)}
		]

	end, Docs),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Actions"}
	] ++ lists:map(fun(#field {label=Label}) ->
		#dcell {type=header, val=Label}
	end, anptest:fs(search)) ++ [
		#dcell {type=header, val="Files"}
	],

	{
		D#dig {
			total=?INFINITY,
			actions=[
				{action_import, "+ Import", "+ Import"}
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



%..............................................................................
%
% layout - files
%
%..............................................................................

layout_files(Doc) ->

	%
	% init
	%
	Names = attachment:get_names(Doc),


	%
	% layout
	%
	lists:map(fun(Name) ->
		#button {
			style="display:block;",
			class="btn btn-link",
			text=Name,
			postback={download, itf:idval(Doc), Name}
		}
	end, Names).


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({download, DocId, AttachmentName}) ->
	attachment:download(anptests:getdb(), DocId, AttachmentName);

event(E) ->
	dig_mm:event(E).

start_upload_event(Event) ->
	dig_mm:start_upload_event(Event).

finish_upload_event(Tag, AttachmentName, LocalFileData, Node) ->
	dig_mm:finish_upload_event(Tag, AttachmentName, LocalFileData, Node).


%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
