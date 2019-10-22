
-module(dig_ep_osm_exam_bundle).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Exams - Bundles").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, ?APPOSM_SCANUPLOADER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=anptest:fs(search)
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exams - Bundles").



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
		% get bundle docs
		%
		Bundles = get_bundles(),


		%
		% layout cells
		%
		FsDoc = itf:d2f(Doc, anptest:fs(search)),
		lists:map(fun(F) ->
			#dcell {val=itl:render(F)}
		end, FsDoc) ++ [
			#dcell {val=get_bundles_count(Bundles, inwardstate)},
			#dcell {val=get_bundles_count(Bundles, scanningstate)},
			#dcell {val=get_bundles_count(Bundles, uploadstate)},
			#dcell {val=itl:btn_group([
				#link {
					text="Inward",
					new=true,
					url=io_lib:format("/dig_ep_osm_exam_inward?id=~s", [itf:idval(Doc)])
				}
			])}
		]

	end, Docs),


	%
	% header
	%
	Header = lists:map(fun(#field {label=Label}) ->
		#dcell {type=header, val=Label}
	end, anptest:fs(search)) ++ [
		#dcell {type=header, val="Inward Complete"},
		#dcell {type=header, val="Scanning Complete"},
		#dcell {type=header, val="Upload Complete"},
		#dcell {type=header, val="Actions"}
	],

	{
		D#dig {
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



get_bundles() ->

	%
	% init
	%
	ExamId = wf:q(id),


	%
	% find docs
	%
	FsToSearch = [
		itf:build(itf:textbox(?F(osm_exam_fk)), ExamId)
	],
	Db2FindRec = db2_find:getrecord_by_fs(
		ep_osm_bundle_api:db(), FsToSearch, 0, ?INFINITY
	),
	#db2_find_response {docs=Docs} = db2_find:find(Db2FindRec#db2_find {
		fields=[
			?OSMBDL(inwardstate),
			?OSMBDL(scanningstate),
			?OSMBDL(uploadstate)
		]
	}),


	%
	% return docs
	%
	Docs.



get_bundles_count(Bundles, StateFId) ->
	DocsComplete = lists:filter(fun(BDoc) ->
		itf:val(BDoc, StateFId) == "completed"
	end, Bundles),
	length(DocsComplete).




%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
