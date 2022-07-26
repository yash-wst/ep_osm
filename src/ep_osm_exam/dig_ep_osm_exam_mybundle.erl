
-module(dig_ep_osm_exam_mybundle).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("My Bundles").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?ADMIN) -> true;
access(_, ?APPOSM_SCANUPLOADER) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, ?APPOSM_QC) -> true;
access(_, _) -> false.


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(myassignment) ->
	itf:dropdown(?F(myassignment, "My Role"), itf:options([
		?F(createdby, "Created By"),
		?F(scannedby, "Scanning Assigned"),
		?F(qualityby, "QC / Upload Assigned"),
		?F(both, "Both")
	])).


%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=[
			itf:build(?COREXS(season_fk), get_active_season_id()),
			?OSMBDL(osm_exam_fk),
			itf:build(f(myassignment), get_default_myassignment()),
			?OSMBDL(scanningstate),
			?OSMBDL(uploadstate),
			?OSMBDL(inward_date),
			?OSMBDL(scanned_date),
			?OSMBDL(uploaded_date)
		],
		config=[
			{searchbar_visibility, "show"},
			{responsive_type, scroll}
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	title().



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
	% fs to filter by my username
	%
	FsMe = case itf:val2(Fs, myassignment) of
		"createdby" -> [
			itf:build(?OSMBDL(createdby), itxauth:user())
		];
		"scannedby" -> [
			itf:build(?OSMBDL(scannedby), itxauth:user())
		];
		"qualityby" -> [
			itf:build(?OSMBDL(qualityby), itxauth:user())
		];
		_ -> [
			itf:build(?OSMBDL(scannedby), itxauth:user()),
			itf:build(?OSMBDL(qualityby), itxauth:user())
		]
	end,
	FsFind = itf:fs_delete(Fs, f(myassignment)) ++ FsMe ++ [
		?OSMBDL(inward_date, #field {db2sort=?SORT_ASC})
	],


	%
	% fetch docs from db
	%
	Docs = ep_osm_bundle_api:fetch(From, Size, FsFind, [
		{use_index, ["season_fk"]}
	]),


	%
	% layout docs
	%
	FsForm = ep_osm_bundle:fs(mybundle),
	Results = lists:map(fun(Doc) ->

		%
		% init
		%
		FsDoc = itf:d2f(Doc, FsForm),
		ExamId = itf:val(Doc, osm_exam_fk),
		BundleId = itf:idval(Doc),


		lists:map(fun(F) ->
			case F#field.id of
				Id when Id == scannedby; Id == qualityby ->
					#dcell {val=itf:val(F)};
				_ ->
					#dcell {val=itl:render(F)}
			end
		end, FsDoc) ++ [
			#dcell {val=#link {
				new=true,
				text="View",
				url=itx:format("/~p?id=~s&digx=~s", [
					dig_ep_osm_exam_inward,
					ExamId,
					base64:encode_to_string(helper:t2l([
						{osm_exam_fk, ExamId},
						{osm_bundle_fk, BundleId}
					]))
				])
			}}
		]
	end, Docs),


	%
	% header
	%
	Header = lists:map(fun(F) ->
		#dcell {type=header, val=F#field.label}
	end, FsForm) ++ [
		#dcell {type=header, val="View"}
	],


	%
	% return layout
	%
	{
		D#dig {
			total=?INFINITY
		},
		[Header | Results]
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

get_active_season_id() ->
	case ep_core_exam_season_api:list_active() of
		[{SeasonId, _} | _] ->
			SeasonId;
		_ ->
			[]
	end.


get_default_myassignment() ->
	get_default_myassignment(itxauth:role()).

get_default_myassignment(?APPOSM_RECEIVER) ->
	"createdby";
get_default_myassignment(_) ->
	"scannedby".

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
