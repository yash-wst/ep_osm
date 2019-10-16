
-module(dig_ep_osm_exam_inward).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Inward").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(osm_exam_fk = I) ->
	F = itf:textbox(?F(I, "OSM Exam")),
	F#field {
		renderer=fun(_Mode, _Event, #field {label=L, uivalue=Id}) ->
				{L, [
					#hidden {id=I, text=Id},
					layout_osm_exam_name(Id)
				]}
		end
	}.



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_RECEIVER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	%
	% init
	%
	OsmExamId = wf:q(id),


	#dig {
		module=?MODULE,
		filters=[
			itf:build(f(osm_exam_fk), OsmExamId),
			?OSMBDL({osm_bundle_fk, OsmExamId})
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Inward").



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
% [osm_exam_fk]
%
%..............................................................................
fetch(D, _From, _Size, [
	#field {id=osm_exam_fk, uivalue=OsmExamId}
]) ->

	%
	% init
	%


	%
	% results
	%
	Results = [],


	%
	% actions
	%
	Actions = [
		{create_bundle, "Create New Bundle", "Create New Bundle"}
	],



	%
	% return
	%
	{D#dig {
		actions=Actions
	}, Results};



%..............................................................................
%
% []
%
%..............................................................................
fetch(D, _From, _Size, Fs) ->
	?D(Fs),
	{D, []}.


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

event({confirmation_yes, create_bundle}) ->
	handle_create_bundle(wf:q(osm_exam_fk));


event(create_bundle) ->
	itl:confirmation(
		#panel {class="mycenter", body=[
			#p {text="Are you sure you want to create a new bundle?"},
			#p {text="Please create new bundle only after previous bundle is full"}
		]},
		create_bundle
	);


event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - create bundle
%
%..............................................................................

handle_create_bundle(ExamId) ->

	%
	% init
	%
	FsToSave = [
		itf:build(?OSMBDL(osm_exam_fk), ExamId),
		itf:build(?OSMBDL(createdby), itxauth:user()),
		itf:build(?OSMBDL(createdon), helper:epochtimestr())
	],


	%
	% save
	%
	case ep_osm_bundle_api:create(FsToSave) of
		{ok, _BundleDoc} ->
			dig:refresh();
		_ ->
			helper_ui:flash(error, "Sorry, could not create bundle!")
	end.


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


layout_osm_exam_name(undefined) ->
	[];
layout_osm_exam_name(OsmExamId) ->
	%
	% init
	%
	{ok, ExamDoc} = anptests:getdoc(OsmExamId),
	itl:blockquote([
		ep_core_exam_season_api:getname(itf:val(ExamDoc, season_fk)),
		itf:val(ExamDoc, anptestcourseid),
		itf:val(ExamDoc, testname),
		?LN(?L2A(itf:val(ExamDoc, teststatus))),
		itf:val(ExamDoc, exam_pattern)
	]).



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
