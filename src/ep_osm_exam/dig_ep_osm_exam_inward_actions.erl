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
		{User, []} ->
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
		{User, []} -> [
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


