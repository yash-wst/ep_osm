-module(ep_osm_bundle_fields).
-compile(export_all).
-include("records.hrl").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(osm_exam_fk = I) ->
	F = itf:textbox_picker(?F(I, "OSM Exam")),
	F#field {
		options=#search {
			title=?LN("Select OSM Exam"),
			db=anptests:getdb(),
			displayfs=anptest:fs(search),
			filterfs=anptest:fs(search),
			size=10
		}
	};

f(number = I) ->
	itf:textbox_int(?F(I, "Bundle Number"));

f(createdby = I) ->
	itf:createdby(?F(I, "Created By"));

f(createdon = I) ->
	itf:createdon(?F(I, "Created On"));

f({osm_bundle_fk = I, OsmExamId}) ->


	%
	% init
	%
	Fs = [
		itf:build(?OSMBDL(osm_exam_fk), OsmExamId)
	],


	%
	% get bundles for the specified exam season
	%
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		ep_osm_bundle_api:db(), Fs, 0, ?INFINITY
	),


	%
	% build options
	%
	Options = itf:options(lists:map(fun(D) ->
		{
			itf:idval(D),
			io_lib:format("~s - ~s", [itf:val(D, number), itf:val(D, createdby)])
		}
	end, Docs)),


	%
	% return dropdown
	%
	itf:dropdown(?F(I, "Bundle"), Options);



f(O) -> throw(O).

%------------------------------------------------------------------------------
% validators
%------------------------------------------------------------------------------
validator(O) ->
	throw(O).

%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------
options(gender) ->
	itf:options([
		?F(male),
		?F(female),
		?F(other)
	]).

%------------------------------------------------------------------------------
% renderers
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

