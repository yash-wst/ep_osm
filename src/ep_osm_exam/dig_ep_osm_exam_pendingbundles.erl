
-module(dig_ep_osm_exam_pendingbundles).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("Pending Bundles").

heading() ->
	title().


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?ADMIN) -> true;
access(_, ?APPOSM_SCANUPLOADER) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, ?APPOSM_QC) -> true;
access(_, _) -> false.


%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		module=?MODULE,
		filters=[
			itf:build(?COREXS(season_fk), get_active_season_id())
		],
		config=[
			{responsive_type, scroll}
		]
	}.

%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------


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
fetch(D, From, Size, Fs) ->

	%
	% Get Fs based on role, containing multiple OR conditions
	%
	FsAuthBased = fsfind(itxauth:role()),

	%
	% Get list of Bundles docs for each set of Fs
	% to emulate an OR operation
	%
	BundleDocs = lists:foldl( fun(FsFind1, Acc) ->
		%
		% get Fs for each OR condition
		%
		FsFind = Fs ++ FsFind1 ++ [
			?OSMBDL(inward_date, #field {db2sort=?SORT_ASC})
		],
		%
		% get bunldes matching each OR condition
		%
		Acc ++ ep_osm_bundle_api:fetch(From, Size, FsFind,
		 	[{use_index, ["inward_date"]}])

		end, [], FsAuthBased),

	%
	% remove duplicate docs, since some docs can match multiple OR conditions
	%
	UniqueDocs = helper:unique(BundleDocs),

	%
	% layout sorted bundles
	%
	FsForm = ep_osm_bundle:fs(pendingbundles),
	Results = lists:map(fun(BundleDoc) ->

		%
		% init useful bundle values
		%
		FsDoc = itf:d2f(BundleDoc, FsForm),
		ExamId = itf:val(BundleDoc, osm_exam_fk),
		BundleId = itf:idval(BundleDoc),

		%
		% Make each table cell corresponding to FsForm schema
		%
		lists:map(fun(F) ->
			#dcell {val=get_dcell(F, BundleDoc)}
		end, FsDoc) ++

		%
		% button to open bundle
		%
		[
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

	end, UniqueDocs),


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
% events
%------------------------------------------------------------------------------

event({confirmation_yes, {Type, BundleDoc}}) ->
	dig_ep_osm_exam_inward_handler:handle_assign_bundle(Type, BundleDoc);


event({assign_bundle, State, BundleDoc}) ->
	Type = case State of
		scanningstate -> scannedby;
		uploadstate -> qualityby;
		qcstate -> qcby
	end,
	dig_ep_osm_exam_inward:event({assign_bundle, Type, BundleDoc});

event({itx, E}) ->
	ite:event(E).


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
% fsfind
%------------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% receiver will get bundles that are created but not marked inward completed
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fsfind(?APPOSM_RECEIVER) -> [
	[
		itf:build(?OSMBDL(createdby), itxauth:user()),
		db2es_find:get_field_cond("$in", inwardstate, ["", "new"])
	]
];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Scanner uploader will get bunldes that are
% 1. maked as inward completed but not yet assigned to anybody for scanning
% 2. marked as scanning completed but not yet assinged to anyone for uploading
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fsfind(?APPOSM_SCANUPLOADER) -> [
	[
		itf:build(itf:hidden(?F(inwardstate)), "completed"),
		db2es_find:get_field_cond("$in", scanningstate, ["","new"])
	],
	[
		itf:build(itf:hidden(?F(inwardstate)), "completed"),
		db2es_find:get_field_cond("$in", uploadstate, ["","new"])
	]
];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% QC user will see bundles that are scanning and uploading completed but
% need to be marked QC complete
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fsfind(?APPOSM_QC) ->[
	[
		itf:build(?OSMBDL(uploadstate), "completed"),
		db2es_find:get_field_cond("$in", qcstate, ["","new"])
	]
].


%------------------------------------------------------------------------------
% misc | utilitiy funcs
%------------------------------------------------------------------------------

get_active_season_id() ->
	case ep_core_exam_season_api:list_active() of
		[{SeasonId, _} | _] ->
			SeasonId;
		_ ->
			[]
	end.



%
% get dcell
%
get_dcell(#field {id=FId} = F, BundleDoc) when
	FId == scanningstate;
	FId == uploadstate;
	FId == qcstate ->
	get_assign_button_based_on_role(
		itxauth:role(), F, itf:val(BundleDoc, FId), BundleDoc
	);
get_dcell(F, _BundleDoc) ->
	itl:render(F).



%
% get assign button
%
get_assign_button_based_on_role(?APPOSM_RECEIVER, _, _, _) ->
	[];
get_assign_button_based_on_role(?APPOSM_SCANUPLOADER, #field {id=scanningstate}, Status, BundleDoc) when
	Status == []; Status == ?NEW ->
	case itf:val(BundleDoc, inwardstate) of
		?COMPLETED ->
			get_assign_button_for_id(scanningstate, BundleDoc);
		_ ->
			[]
	end;
get_assign_button_based_on_role(?APPOSM_SCANUPLOADER, #field {id=uploadstate}, Status, BundleDoc) when
	Status == []; Status == ?NEW ->
	case itf:val(BundleDoc, scanningstate) of
		?COMPLETED ->
			get_assign_button_for_id(uploadstate, BundleDoc);
		_ ->
			[]
	end;
get_assign_button_based_on_role(?APPOSM_QC, #field {id=qcstate}, Status, BundleDoc) when
	Status == []; Status == ?NEW ->
	case itf:val(BundleDoc, uploadstate) of
		?COMPLETED ->
			get_assign_button_for_id(qcstate, BundleDoc);
		_ ->
			[]
	end;
get_assign_button_based_on_role(_, F, _, _) ->
	itl:render(F).




%
% get button
%
get_assign_button_for_id(Id) ->
	ite:button(
		assign_bundle, "Assign", {assign_bundle, Id, BundleDoc}, "btn btn-info"
	).


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
