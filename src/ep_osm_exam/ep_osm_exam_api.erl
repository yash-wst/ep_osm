
-module(ep_osm_exam_api).
-compile(export_all).
-include("records.hrl").

db() ->
	"anptests".

%------------------------------------------------------------------------------
% get
%------------------------------------------------------------------------------
get(Id) ->
	db:get(db(), Id).

%
% assuming a view for the specified field exists
%
get_by_field(F = #field {}) ->
	itxview:find_docs(db(), F).



getdocs() ->
	db:getdocs(db()).


getdocs_by_ids(ExamIds) ->
	db:get_docs_by_ids(db(), ExamIds).



list() ->
	lists:map(fun(D) ->
		{helper:l2a(itf:idval(D)), itf:idval(D)}
	end, db:getdocs(db())).


%------------------------------------------------------------------------------
% check
%------------------------------------------------------------------------------
exists(F = #field {}) ->
	{Count, _} = get_by_field(F),
	Count > 0.


%------------------------------------------------------------------------------
% save
%------------------------------------------------------------------------------
save(Fields) ->
	db:save(db(), helper_api:fields2doc(Fields)).

save(FsToSave, FsAll, Id) ->
	{ok, Doc} = ?MODULE:get(Id),
	FsAll1 = itf:d2f(Doc, FsAll),
	FsAll2 = itf:fs_merge(FsAll1, FsToSave),
	?MODULE:save(FsAll2).

savebulk(LoLofFields) ->

	%
	% save docs
	%
	Docs = lists:map(fun(Fs) ->
		helper_api:fields2doc(Fs)
	end, LoLofFields),
	{ok, ResDocs} = db:savebulk(db(), Docs),


	%
	% setup
	%
	lists:foreach(fun(Doc) ->
		Id = itf:val(Doc, id),
		anptests:setup(Id)
	end, ResDocs),



	%
	% return
	%
	{ok, ResDocs}.


%------------------------------------------------------------------------------
% mutate
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% fetch
%------------------------------------------------------------------------------

fetch(From, Size, Filters) ->
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(db(), Filters, From, Size),
	Docs.


%------------------------------------------------------------------------------
% delete
%------------------------------------------------------------------------------
delete_by_field(F = #field {}) ->
	{_, Docs} = get_by_field(F),
	lists:map(fun(D) ->
		db:delete_id(db(), itf:idval(D))
	end, Docs).




%------------------------------------------------------------------------------
% stats
%------------------------------------------------------------------------------


get_stats([]) ->
	lists:foldl(fun(State, Acc) ->
		SK = [helper:l2b(State), <<"">>, <<"">>, <<"">>, <<"">>],
		EK = [helper:l2b(State), <<"z\\ufff0">>, <<"z\\ufff0">>, <<"z\\ufff0">>, <<"z\\ufff0">>],
		Acc ++ itxview:get_stats(
			db(), "teststatus_season_fk_faculty_code_fk_program_code_fk_subject_code_fk", SK, EK, 2
		)
	end, [], states());


get_stats([
	#field {id=season_fk, uivalue=SeasonId}
]) ->
	lists:foldl(fun(State, Acc) ->
		SK = [helper:l2b(State), helper:l2b(SeasonId), <<"">>, <<"">>, <<"">>],
		EK = [helper:l2b(State), helper:l2b(SeasonId), <<"z\\ufff0">>, <<"z\\ufff0">>, <<"z\\ufff0">>],
		Acc ++ itxview:get_stats(
			db(), "teststatus_season_fk_faculty_code_fk_program_code_fk_subject_code_fk", SK, EK, 3
		)
	end, [], states());


get_stats([
	#field {id=season_fk, uivalue=SeasonId},
	#field {id=faculty_code_fk, uivalue=FacultyId}
]) ->
	lists:foldl(fun(State, Acc) ->
		SK = [helper:l2b(State), helper:l2b(SeasonId), helper:l2b(FacultyId), <<"">>, <<"">>],
		EK = [helper:l2b(State), helper:l2b(SeasonId), helper:l2b(FacultyId), <<"z\\ufff0">>, <<"z\\ufff0">>],
		Acc ++ itxview:get_stats(
			db(), "teststatus_season_fk_faculty_code_fk_program_code_fk_subject_code_fk", SK, EK, 4
		)
	end, [], states());


get_stats([
	#field {id=season_fk, uivalue=SeasonId},
	#field {id=faculty_code_fk, uivalue=FacultyId},
	#field {id=program_code_fk, uivalue=ProgramId}
]) ->
	lists:foldl(fun(State, Acc) ->
		SK = [helper:l2b(State), helper:l2b(SeasonId), helper:l2b(FacultyId), helper:l2b(ProgramId), <<"">>],
		EK = [helper:l2b(State), helper:l2b(SeasonId), helper:l2b(FacultyId), helper:l2b(ProgramId), <<"z\\ufff0">>],
		Acc ++ itxview:get_stats(
			db(), "teststatus_season_fk_faculty_code_fk_program_code_fk_subject_code_fk", SK, EK, 5
		)
	end, [], states()).



%------------------------------------------------------------------------------
% stats - evaluation
%------------------------------------------------------------------------------


get_evaluation_stats0(TestId) ->
	SK = [<<"">>, <<"">>],
	EK = [<<"z\\ufff0">>, <<"z\\ufff0">>],
	itxview:get_stats(
		anpcandidates:db(TestId), "state_assigned", SK, EK, 1
	).


get_evaluation_stats(TestId) ->
	SK = [<<"">>, <<"">>],
	EK = [<<"z\\ufff0">>, <<"z\\ufff0">>],

	lists:foldl(fun(Viename, Acc) ->
		Acc ++
		try
			itxview:get_stats(
				anpcandidates:db(TestId), Viename, SK, EK, 2
			)
		catch error:{badmatch,{error,not_found}} ->
			view:setup_anpcandidates(anpcandidates:db(TestId)),
			itxview:get_stats(
				anpcandidates:db(TestId), Viename, SK, EK, 2
			)
		end
	end, [], [
		"state_assigned",
		"state_assigned_anpmoderator",
		"state_assigned_anprevaluator",
		"state_assigned_anpmoderator_reval"
	]).





%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


states() -> [
	"new",
	"scheduled",
	"active",
	"completed",
	"retired",
	"redressal"
].


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

