
-module(dig_osm_candidate_marks).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("OSM Candidate Marks").

heading() ->
	title().


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
		description="OSM Candidate Marks",
		module=?MODULE,
		filters=[
			?COREXS(season_fk),
			?CORFAC(faculty_code_fk),
			?CORPGM(program_code_fk),
			?CORSUB(subject_code_fk),
			fields:get(anptestcourseid),
			fields:get(teststatus),
			fields:get(exam_pattern),
			itf:build(itf:hidden(osm_exam_fk), wf:q(id))
		],
		events=[
			ite:button(email_export, "Email CSV", {itx, {dig, email_export}})
		],
		size=25
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Candidate Marks").



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
fetch(D, _From, _Size, [
		#field {id=name, uivalue=Val}
	]) ->
	{
		D#dig {
			description=Val
		},
		[
			[
				#dcell {val="Ram"},
				#dcell {val="Kulkarni"}
			],
			[
				#dcell {val="Vishal"},
				#dcell {val="Ghadge"}
			]
		]
	};


%..............................................................................
%
% [other]
%
%..............................................................................
fetch(D, _From, _Size, _) ->
	{
		D,
		[{error, "This combination of filters has not been implemented.
		If you think it is useful, please contact the support team."}]
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



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
