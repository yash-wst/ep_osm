
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

	FsFind = itf:fs_delete(Fs, f(myassignment)),

	%
	% fetch docs from db
	%
	BundleDocs = ep_osm_bundle_api:fetch(From, Size, FsFind, [
		{use_index, ["inward_date"]}
	]),


	%
	% sort bundles by inward date
	%
	BundleDocsSorted = lists:sort(fun(A, B) ->
		itf:val(A, inward_date) < itf:val(B, inward_date)
	end, BundleDocs),


	%
	% filter out entries with both scanning + uploading completed
	%

	PendingBundlesSorted = [ X  || X <- BundleDocsSorted,
		true == isBundlePendingForScanOrUpload(X) ],

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
			case F#field.id of
				Id when Id == scannedby; Id == qualityby; Id == qcby ->
					#dcell {  val=itf:val(F) };

				State when State == scanningstate;
						 State == uploadstate;
						 State == qcstate ->
					#dcell { val= case itf:val(F) of
							"completed" -> "Completed";
							"assigned" -> "Assigned";
							_ -> get_assign_button_based_on_role(
									itxauth:role(),
									State,
									BundleDoc)
							end
						};
				_ ->
					#dcell {val=itl:render(F)}
			end
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

	end, PendingBundlesSorted),


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
	dig:dig(?MODULE:get(itxauth:role())).



%------------------------------------------------------------------------------
% get
%------------------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% receiver will get bundles that are created but not marked inward completed
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(?APPOSM_RECEIVER) ->
	#dig {
		module=?MODULE,
		filters=[
			itf:build(?COREXS(season_fk), get_active_season_id()),
			?OSMBDL(osm_exam_fk),
			itf:build(f(myassignment), get_default_myassignment()),
			itf:build(?OSMBDL(inwardstate), "new")
		],
		config=[
			{searchbar_visibility, "show"},
			{responsive_type, scroll}
		]
	};



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Scanner uploader will get bunldes that are
% 1. maked as inward completed but not yet assigned to anybody for scanning
% 2. marked as scanning completed but not yet assinged to anyone for uploading
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(?APPOSM_SCANUPLOADER) ->

	#dig {
		module=?MODULE,
		filters=[
			itf:build(?COREXS(season_fk), get_active_season_id()),
			?OSMBDL(osm_exam_fk),
			itf:build(?OSMBDL(inwardstate), "completed"),
			?OSMBDL(scanningstate),
			?OSMBDL(uploadstate)
		],
		config=[
			{searchbar_visibility, "show"},
			{responsive_type, scroll}
		]
	};



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% QC user will see bundles that are scanning and uploading completed but
% need to be marked QC complete
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(?APPOSM_QC) ->
	#dig {
		module=?MODULE,
		filters=[
			itf:build(?COREXS(season_fk), get_active_season_id()),
			?OSMBDL(osm_exam_fk),
			itf:build(?OSMBDL(inwardstate), "completed"),
			itf:build(?OSMBDL(scanningstate), "completed"),
			itf:build(?OSMBDL(uploadstate), "completed"),
			itf:build(?OSMBDL(qcstate), "new")
		],
		config=[
			{searchbar_visibility, "show"},
			{responsive_type, scroll}
		]
	};


get(_) ->
	ok.


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

get_default_myassignment() ->
	get_default_myassignment(itxauth:role()).

get_default_myassignment(?APPOSM_RECEIVER) ->
	"createdby";
get_default_myassignment(?APPOSM_QC) ->
	"qcby";
get_default_myassignment(?APPOSM_SCANUPLOADER) ->
	"scannedby";
get_default_myassignment(_) ->
	"scannedby".

isBundlePendingForScanOrUpload(BundleDoc) ->
	UploadState = itf:val(BundleDoc, uploadstate),
	ScanningState = itf:val(BundleDoc, scanningstate),

	case ( ScanningState /= "completed" orelse UploadState /= "completed")  of
		true -> true;
		_ -> false
	end.

get_assign_button_based_on_role(Role, State, BundleDoc) ->
	 case Role of
	 	?APPOSM_RECEIVER -> [];
	 	?APPOSM_SCANUPLOADER ->
	 		case State /= qcstate of
	 			true ->
			 		ite:button(assign_bundle,
					 	"Assign",
					 	{assign_bundle, State, BundleDoc},
					 	"btn btn-info");
			 	_ -> []
			 end;
		?APPOSM_QC ->
			case State == qcstate of
				true ->
					ite:button(assign_bundle,
					 	"Assign",
					 	{assign_bundle, State, BundleDoc},
					 	"btn btn-info");
				_ -> []
			end
    end.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
