-module(ep_osm_candidate_dtp_marks_omr_import).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").



-define(POS_UID, 1).
-define(POS_MRK, 2).


%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% handle import validate csv list
%------------------------------------------------------------------------------

handle_import_validate(List) ->

	%
	% init
	%
	ExamId = minijobcontext:q(id),

	ok = dig_mm_import_validator:handle_import_validate_csv_length(List, 2),
	ok = dig_mm_import_validator:handle_import_validate_csv_non_empty(List),
	ok = dig_mm_import_validator:handle_import_validate_unique_primary_keys(List, ?POS_UID),
	ok = dig_mm_import_validator:handle_import_validate_field(
		List, ?POS_MRK, validators:get(number)
	),
	ok = dig_mm_import_validator:handle_import_validate_entities_exist(
		List, ?POS_UID, ep_osm_candidate_api:db(ExamId), anp_paper_uid
	),
	ok.



%------------------------------------------------------------------------------
% handle import validate batch
%------------------------------------------------------------------------------

handle_import_validate_batch(_List) ->
	ok.


%------------------------------------------------------------------------------
% handle import csv to fs - imports student master data
%------------------------------------------------------------------------------

handle_import_csv_to_fs(List) ->

	%
	% init
	%
	UIds = [lists:nth(1, Line) || Line <- List],
	ExamId = minijobcontext:q(id),
	ExamDb = ep_osm_candidate_api:db(ExamId),
	Docs = db2_find:getdocs_by_ids(ExamDb, anp_paper_uid, UIds),
	DocsDict = helper:get_dict_from_docs(Docs, anp_paper_uid),

	%
	% update fields
	%
	lists:map(fun([
		AnpPaperUId,
		Marks
	]) ->

		%
		% init
		%
		{ok, Doc} = dict:find(AnpPaperUId, DocsDict),
		Fs = itf:d2f(Doc, ep_osm_candidate:fs(all)),

		%
		% fs to save
		%
		FsToSave = [
			fields:build(anp_paper_uid, AnpPaperUId),
			fields:build(dtp_marks_omr, Marks),
			itf:build(fields:get(anpstate), "anpstate_completed")
		],

		%
		% change list
		%
		Changelist = itf:fs_changelist(Doc, FsToSave),
		FComment = itf:d2f(Doc, fields:get(comments_dtp)),
		FComment1 = itf:build_comment(FComment, Changelist), 


		%
		% return full and final fs
		%
		itf:fs_merge(Fs, FsToSave ++ [FComment1])

	end, List).




%------------------------------------------------------------------------------
% db
%------------------------------------------------------------------------------

savebulk(LoLofFields) ->
	ExamId = minijobcontext:q(id),
	ExamDb = ep_osm_candidate_api:db(ExamId),
	LoLDocs = [helper_api:fields2doc(Fs) || Fs <- LoLofFields],
	db:savebulk(ExamDb, LoLDocs, 250).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
