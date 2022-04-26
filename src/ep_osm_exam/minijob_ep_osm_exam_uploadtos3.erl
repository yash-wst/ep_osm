
-module(minijob_ep_osm_exam_uploadtos3).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(itx, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered.html"})).

title() ->
	?LN("minijob_ep_osm_exam_uploadtos3").

heading() ->
	title().


%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(?VIEW, _) -> true;
access(_, ?ADMIN) -> true;
access(_, _) -> false.


%------------------------------------------------------------------------------
% fields desc
%------------------------------------------------------------------------------

f(objectkey) ->
	itf:textbox(?F(objectkey, "Object Key"));


f(aws_s3_dir) ->
	itf:textbox(?F(aws_s3_dir, "S3 Dir")).


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------



%
% fs - doc
%
fs(Doc) ->

	%
	% init
	%
	OsmExamId = itf:val(Doc, osm_exam_fk),



	%
	% fs
	%
	[
		?OSMEXM(osm_exam_fk),
		?OSMBDL({osm_bundle_fk, OsmExamId}),
		f(objectkey),
		f(aws_s3_dir)
	].



%
% fs -
%
fs() ->
	[
		?OSMEXM(osm_exam_fk),
		f(objectkey),
		f(aws_s3_dir)
	].


%------------------------------------------------------------------------------
% concurrency
%------------------------------------------------------------------------------

concurrency() ->
	one_per_user.



%------------------------------------------------------------------------------
% size
%------------------------------------------------------------------------------

size(_Doc) ->
	100.


%------------------------------------------------------------------------------
% progress
%------------------------------------------------------------------------------
progress(Doc) ->
	progress(Doc, 1).
progress(Doc, Increment) ->
	minijob_api:update_progress(Doc, Increment).



%------------------------------------------------------------------------------
% layouts
%------------------------------------------------------------------------------

layout() ->
	minijob:layout(?MODULE).


layout_previous_uploads(BundleDoc) ->

	%
	% filters
	%
	Filters = [
		itf:build(?MJOB(mj_module), "minijob_ep_osm_exam_uploadtos3"),
		fields:build(osm_bundle_fk, itf:idval(BundleDoc))
	],

	%
	% find
	%
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		minijob_api:db(), Filters, 0, 100, [
			{use_index, ["mj_module"]}
		]
	),
	DocsSorted = lists:sort(fun(A, B) ->
		helper:s2n(itf:val(A, mj_createdon)) >
		helper:s2n(itf:val(B, mj_createdon))
	end, Docs),

	%
	% get values
	%
	FsGrid = minijob:fs(grid),
	LoL = lists:map(fun(Doc) ->
		Fs = itf:d2f(Doc, FsGrid),
		lists:map(fun(F) ->
			itl:render(F)
		end, Fs) ++ [
			#link {
				new=true,
				text="View",
				url=itx:format("/~p?mode=view&id=~s", [
					?MODULE, itf:idval(Doc)
				])
			}
		]
	end, DocsSorted),


	%
	% header
	%
	Header = lists:map(fun(F) ->
		F#field.label
	end, FsGrid) ++ [
		"Details"
	],


	%
	%
	%
	dig:layout_vals(#dig{}, LoL, Header).


%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------
event(E) ->
	minijob:event(?MODULE, E).



%------------------------------------------------------------------------------
% create and run
%------------------------------------------------------------------------------

create_and_run(Fs) ->
	minijob:create_and_run(?MODULE, Fs).



%------------------------------------------------------------------------------
% do
%------------------------------------------------------------------------------

do(Doc) ->

	%
	% init
	%
	OsmExamId = itf:val(Doc, osm_exam_fk),
	OsmBundleId = itf:val(Doc, osm_bundle_fk),
	S3Dir = itf:val(Doc, aws_s3_dir),
	ObjectKey = itf:val(Doc, objectkey),
	BundleDocs = dig_ep_osm_exam_inward:get_bundle_docs(OsmExamId, OsmBundleId),


	%
	% dirs to upload
	%
	DirNamesToUpload = lists:map(fun(D) ->
		itf:val(D, anpseatnumber)
	end, BundleDocs),



	%
	% upload
	%
	dig_ep_osm_exam_inward_uploadtos3:upload2(
		undefined, S3Dir, DirNamesToUpload, ObjectKey, undefined, OsmBundleId
	).


%------------------------------------------------------------------------------
% commit
%------------------------------------------------------------------------------

commit() ->
	ok.


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

