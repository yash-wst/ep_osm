
-module(dig_ep_osm_exam_results).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("OSM Exam Results").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% ids
%------------------------------------------------------------------------------

exportids() -> [
	"season_code",
	"season_name",
	"program_code",
	"program_name",
	"subject_code",
	"subject_name",
	"prn",
	"seatnumber",
	"booklet_number",
	"sticker_uid",
	"courseid",
	"evaluator_total",
	"moderator_total",
	"revaluator_total"
].


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------

f(exportids) ->
	itf:build(itf:textarea(?F(exportids, "Export IDs")), string:join(exportids(), "\n"));

f("season_" ++ Id) ->
	F = ?COREXS(?L2A(Id)),
	F#field {label="Season " ++ F#field.label};

f("program_" ++ _ = Id) ->
	?CORPGM(?L2A(Id));

f("subject_" ++ _ = Id) ->
	?CORSUB(?L2A(Id));

f("prn") ->
	itf:textbox(?F(prn, "PRN"));

f("booklet_number") ->
	itf:textbox(?F(booklet_number, "Booklet Number"));

f("sticker_uid") ->
	itf:textbox(?F(sticker_uid, "Sticker UId"));

f("seatnumber") ->
	fields:get(anpseatnumber);

f("evaluator_total") ->
	fields:get(total_anpevaluator);

f("moderator_total") ->
	fields:get(total_anpmoderator);

f("revaluator_total") ->
	fields:get(total_anprevaluator);

f("courseid") ->
	fields:get(anptestcourseid);

f(Id) ->
	fields:get(?L2A(Id)).

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
		module=?MODULE,
		filters=[
			?COREXS(season_fk),
			?CORFAC(faculty_code_fk),
			?CORPGM(program_code_fk),
			?CORSUB(subject_code_fk),
			fields:get(anptestcourseid),
			fields:get(teststatus),
			fields:get(exam_pattern),
			itf:build(itf:hidden(osm_exam_fk), wf:q(id)),
			f(exportids)
		],
		size=100,
		actions=[
			{export_results_bulk, "Bulk Export Results", "Bulk Export Results"}
		],
		events=[
			ite:button(export, "CSV", {itx, {dig, export}})
		]
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exam Results").



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
	#field {id=exportids}
]) ->
	{D, []};


%..............................................................................
%
% [osm_exam_fk]
%
%..............................................................................
fetch(D, From, Size, [
	#field {id=osm_exam_fk, uivalue=ExamId},
	#field {id=exportids, uivalue=ExportIds}
]) ->


	%
	% init
	%
	ExportIds1 = string:tokens(ExportIds, "\n"),
	Db = anpcandidates:db(ExamId),
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),
	SeasonId = itf:val(ExamDoc, season_fk),
	ProgramId = itf:val(ExamDoc, program_code_fk),
	SubjectId = itf:val(ExamDoc, subject_code_fk),
	SeatNumberMappingId = itxconfigs_cache:get2(osm_images_folder_id, booklet_number),



	%
	% get docs
	%
	SeasonDoc = itx:okdoc(ep_core_exam_season_api:get(SeasonId)),
	ProgramDoc = itx:okdoc(ep_core_program_api:get(ProgramId)),
	SubjectDoc = itx:okdoc(ep_core_subject_api:get(SubjectId)),


	%
	% get total count
	%
	Count = db:count(Db),


	%
	% get docs
	%
	Docs = anpcandidates:getdocs(Db, From, Size),


	%
	% get corresponding rds docs
	%
	SeatNumbers = lists:map(fun(Doc) ->
		itf:val(Doc, anpseatnumber)
	end, Docs),
	RdsDocs = ep_rds_result_api:get_rds_docs(SeasonId, SubjectId, SeatNumbers, SeatNumberMappingId),
	RdsDocsDict = helper:get_dict_from_docs(RdsDocs, SeatNumberMappingId),



	%
	% layout results
	%
	Results = lists:map(fun(Doc) ->
		SeatNumber = itf:val(Doc, anpseatnumber),
		lists:map(fun(Id) ->
			#dcell {
				val=val(
					ExamDoc, SeasonDoc, ProgramDoc, SubjectDoc,
					Doc, dict:find(SeatNumber, RdsDocsDict), Id
				)
			}
		end, ExportIds1)
	end, Docs),


	%
	% header
	%
	Header = lists:map(fun(Id) ->
		#field {label=Label} = f(Id),
		#dcell {
			type=header,
			val=Label
		}
	end, ExportIds1),


	%
	% return
	%
	{
		D#dig {
			total=Count,
			actions=[]
		},
		[Header] ++ Results
	};



%..............................................................................
%
% default
%
%..............................................................................

fetch(D, From, Size, Fs) ->


	%
	% get active tests
	%
	Fs1 = itf:fs_delete(Fs, #field{id=exportids}),
	Docs = ep_osm_exam_api:fetch(From, Size, Fs1),


	%
	% build dicts
	%
	SeasonDocsDict = ep_core_exam_season_api:get_dict(Docs),
	FacultyDocsDict = ep_core_faculty_api:get_dict(Docs),
	ProgramDocsDict = ep_core_program_api:get_dict(Docs),
	SubjectDocsDict = ep_core_subject_api:get_dict(Docs),


	%
	% results
	%
	Results = lists:map(fun(Doc) ->


		%
		% init
		%
		ExamId = itf:idval(Doc),
		SeasonDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, season_fk), SeasonDocsDict),
		FacultyDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, faculty_code_fk), FacultyDocsDict),
		ProgramDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, program_code_fk), ProgramDocsDict),
		SubjectDoc = helper:get_doc_or_empty_doc_from_dict(itf:val(Doc, subject_code_fk), SubjectDocsDict),


		%
		% layout test
		%
		[
			#dcell {val=itl:blockquote(SeasonDoc, [?COREXS(name), ?COREXS(state)])},
			#dcell {val=itl:blockquote(FacultyDoc, [?CORFAC(faculty_code), ?CORFAC(faculty_name)])},
			#dcell {val=itl:blockquote(ProgramDoc, [?CORPGM(program_code), ?CORPGM(program_name)])},
			#dcell {val=itl:blockquote(SubjectDoc, [?CORSUB(subject_code), ?CORSUB(subject_name)])},
			#dcell {
				val=itl:blockquote([
					#link {
						new=true,
						url=io_lib:format("/~p?id=~s", [
							wf:page_module(), ExamId
						]),
						text=itf:val(Doc, anptestcourseid)
					}
				])
			}
		]

	end, Docs),



	%
	% header
	%
	Header = [
		#dcell {type=header, val="Season"},
		#dcell {type=header, val="Faculty"},
		#dcell {type=header, val="Program"},
		#dcell {type=header, val="Subject"},
		#dcell {type=header, val="Course ID"}
	],



	%
	% return
	%
	{
		D#dig {
			total=?INFINITY
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


%
% val
%
val(ExamDoc, _SeasonDoc, _ProgramDoc, _SubjectDoc, _Doc, _RdsDoc, Id) when
	Id == "courseid" ->
	itf:val(ExamDoc, f(Id));
val(_ExamDoc, SeasonDoc, _ProgramDoc, _SubjectDoc, _Doc, _RdsDoc, Id) when
	Id == "season_name";
	Id == "season_code" ->
	itf:val(SeasonDoc, f(Id));
val(_ExamDoc, _SeasonDoc, ProgramDoc, _SubjectDoc, _Doc, _RdsDoc, Id) when
	Id == "program_name";
	Id == "program_code" ->
	itf:val(ProgramDoc, f(Id));
val(_ExamDoc, _SeasonDoc, _ProgramDoc, SubjectDoc, _Doc, _RdsDoc, Id) when
	Id == "subject_name";
	Id == "subject_code" ->
	itf:val(SubjectDoc, f(Id));
val(_ExamDoc, _SeasonDoc, _ProgramDoc, _SubjectDoc, Doc, _RdsDoc, Id) when
	Id == "evaluator_total";
	Id == "moderator_total";
	Id == "revaluator_total" ->
	Val = itf:val(Doc, f(Id)),
	case Val of
		[] ->
			[];
		_ -> helper:i2s(helper:ceiling(helper:s2f_v1(Val)))
	end;
val(_ExamDoc, _SeasonDoc, _ProgramDoc, _SubjectDoc, _Doc, {ok, RdsDoc}, Id) when
	Id == "prn";
	Id == "booklet_number";
	Id == "sticker_uid" ->
	itf:val(RdsDoc, f(Id));
val(_ExamDoc, _SeasonDoc, _ProgramDoc, _SubjectDoc, _Doc, error, Id) when
	Id == "prn";
	Id == "booklet_number";
	Id == "sticker_uid" ->
	[];
val(_ExamDoc, _SeasonDoc, _ProgramDoc, _SubjectDoc, Doc, _RdsDoc, Id) ->
	itf:val(Doc, f(Id)).

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
