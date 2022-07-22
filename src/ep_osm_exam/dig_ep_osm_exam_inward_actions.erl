-module(dig_ep_osm_exam_inward_actions).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-import(dig_ep_osm_exam_inward, [
	event/1
]).
-import(dig_ep_osm_exam_inward_handler, [
	handle_uploaded_zip_file/0
]).


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------


%..............................................................................
%
% layout - actions for bundle
%
%..............................................................................

layout_actions_bundle(BundleDoc, _IsBundleActive = true) ->
	layout_action_bundle_default() ++
	layout_action_inwarding(BundleDoc) ++
	layout_action_scanning(BundleDoc) ++
	layout_action_uploading(BundleDoc) ++
	layout_action_qc(BundleDoc) ++
	layout_action_inward_form(BundleDoc) ++
	layout_danger_actions_discard_bundle(BundleDoc, itxauth:role());
layout_actions_bundle(_BundleDoc, _) ->
	itl:disable_page_inputs(),
	helper_ui:flash(error, "Inward disabled for this bundle!"),
	[].




%..............................................................................
%
% layout - actions for exam
%
%..............................................................................

layout_actions_exam(_IsBundleActive = true) ->
	case itxauth:role() of
		?APPOSM_RECEIVER -> [
			{create_bundle_form, "Create New Bundle", "Create New Bundle"},
			{action_import, "+ Import", "+ Import"},
			{import_master_data, "Import Master Data", "Import Master Data"}
		];
		_ -> [
		]
	end;
layout_actions_exam(_) ->
	itl:disable_page_inputs(),
	helper_ui:flash(error, "Inward disabled for this exam!"),
	[].



%..............................................................................
%
% layout - action bundle default
%
%..............................................................................

layout_action_bundle_default() ->
	[
		{print_bundle_cover, "Print Bundle Cover", "Print Bundle Cover"},
		{export_bundle_csv, "Export Bundle CSV", "Export Bundle CSV"},
		{export_bundle_dir, "Export Bundle Folder", "Export Bundle Folder"}
	]. 

%..............................................................................
%
% layout - action inward form
%
%..............................................................................

layout_action_inward_form(BundleDoc) ->

	%
	% init
	%
	User = itxauth:user(),
	handle_uploaded_zip_file(),

	%
	% action
	%
	case {itf:val(BundleDoc, createdby), itf:val(BundleDoc, inwardstate)} of
		{User, State} when State ==[]; State == "new" ->
			event(inward_form),
			[
				{inward_form, "Inward Form", "Inward Form"}
			];
		_ -> [
		]
	end.


%..............................................................................
%
% layout - action inwarding
%
%..............................................................................

layout_action_inwarding(BundleDoc) ->

	%
	% init
	%
	User = itxauth:user(),


	%
	% action
	%
	case {itf:val(BundleDoc, createdby), itf:val(BundleDoc, inwardstate)} of
		{User, State} when State == []; State == "new" -> [
			{inward_completed, "Inward Completed", "Inward Completed"}
		];
		_ -> [
		]
	end.


%..............................................................................
%
% layout - action scanning
%
%..............................................................................

layout_action_scanning(BundleDoc) ->

	%
	% init
	%
	User = itxauth:user(),


	%
	% action
	%
	case {itf:val(BundleDoc, scannedby), itf:val(BundleDoc, scanningstate)} of
		{User, "assigned"} -> [
			{scanning_completed, "Scanning Completed", "Scanning Completed"}
		];
		_ -> [
		]
	end.



%..............................................................................
%
% layout - action uploading
%
%..............................................................................

layout_action_uploading(BundleDoc) ->

	%
	% init
	%
	User = itxauth:user(),


	%
	% action
	%
	case {itf:val(BundleDoc, qualityby), itf:val(BundleDoc, uploadstate)} of
		{User, "assigned"} ->
			event({upload_form, BundleDoc}),
			[
				{upload_form, "Upload Form", "Upload Form"},
				{upload_completed, "Uploading Completed", "Uploading Completed"}
			];
		_ -> [
		]
	end.



%..............................................................................
%
% layout - action qc
%
%..............................................................................

layout_action_qc(BundleDoc) ->
	layout_action_qc(BundleDoc, ep_osm_config:is_qc_enabled()).


layout_action_qc(BundleDoc, true) ->
	%
	% init
	%
	User = itxauth:user(),


	%
	% action
	%
	case {itf:val(BundleDoc, qcby), itf:val(BundleDoc, qcstate)} of
		{User, "assigned"} ->
			[
				{qc_completed, "QC Completed", "QC Completed"}
			];
		_ -> [
		]
	end;
layout_action_qc(_BundleDoc, false) ->
	[].


%..............................................................................
%
% layout - danger actions
%
%..............................................................................


%
% action - discard bundle
%
layout_danger_actions_discard_bundle(BundleDoc, ?APPOSM_RECEIVER) ->

	%
	% init
	%
	case {
		itf:val(BundleDoc, inwardstate), 
		itf:val(BundleDoc, scanningstate), 
		itf:val(BundleDoc, uploadstate)
	} of
		{InwardState, ScanningState, UploadState} when 
			InwardState == "discarded";
			ScanningState == ?COMPLETED;
			UploadState == ?COMPLETED ->
				[];
		{_, _, _} -> [
			{discard_bundle, "Discard Bundle", "Discard Bundle"}
		]
	end;
layout_danger_actions_discard_bundle(_, _) ->
		[].



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------


