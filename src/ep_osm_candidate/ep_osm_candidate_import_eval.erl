-module(ep_osm_candidate_import_eval).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%
% define
%
-define(INDEX_TEST_CODE, 1).
-define(INDEX_DATE, 2).
-define(INDEX_ANP_PRN, 3).
-define(INDEX_ANP_EVALUATOR, 4).
-define(INDEX_ANP_MODERATOR, 5).
-define(INDEX_ANP_REVALUATOR, 6).
-define(INDEX_ANP_MODERATOR_REVALUATOR, 7).
-define(MAX_CSV_COL_SIZE, 3).


fs() -> [
    fields:get(anptestcourseid),
    fields:get(startdate),
    fields:get(anpseatnumber),
    itf:textbox(?F(username_anpevaluator, "Evaluator"), []),
    itf:textbox(?F(username_anpmoderator, "Moderator"), []),
    itf:textbox(?F(username_anprevaluator, "Revaluator"), []),
    itf:textbox(?F(username_anpmoderator_reval, "Moderator Reval"), [])
].


fs(merge) -> [
    fields:get(profileidfk_anpevaluator),
    fields:get(profileidfk_anpmoderator),
    fields:get(profileidfk_anprevaluator),
    fields:get(profileidfk_anpmoderator_reval)
];


fs(import) ->[
    ?COREXS(season_fk, #field {id=import_season_fk}),
    itf:attachment(?F(file_import_student_evaluator_data, "CSV file"))
].


%------------------------------------------------------------------------------
% handle import validate csv list
%------------------------------------------------------------------------------

handle_import_validate(List) ->
    Fun = fun(Csv) ->
        length(Csv) > ?MAX_CSV_COL_SIZE
    end,
    ok = dig_mm_import_validator:handle_import_validate_csv_length(List, Fun),
    ok = ep_osm_candidate_import:handle_import_validate_csv_non_empty(List),
    ok = handle_validate_eval_profile_exists(List),
    ok = handle_import_validate_exams_exist(List),
    ok.


handle_validate_eval_profile_exists(List) ->
    ProfileDocsDict = get_username_dict_by_usertype(List),
    validate_profile_exists(List, anpevaluator, ?INDEX_ANP_EVALUATOR, ProfileDocsDict),
    validate_profile_exists(List, anpmoderator, ?INDEX_ANP_MODERATOR, ProfileDocsDict),
    validate_profile_exists(List, anprevaluator, ?INDEX_ANP_REVALUATOR, ProfileDocsDict),
    validate_profile_exists(List, anpmoderator_reval, ?INDEX_ANP_MODERATOR_REVALUATOR, ProfileDocsDict).


validate_profile_exists(List, ProfileType, ProfileIndex, ProfileDocsDict) ->
    {_, Errors} = lists:foldl(fun(Csv, {AccOk, AccErr}) ->
        case lists:nth(ProfileIndex, Csv) of
            % Ignore if username is NA
            "NA" ->
                {AccOk, AccErr};
            _ ->
                case dict:find({lists:nth(ProfileIndex, Csv), ProfileType}, ProfileDocsDict) of
                    {ok, _} ->
                        {AccOk ++ [{lists:nth(ProfileIndex, Csv), ProfileType}], AccErr};
                    _ ->
                        {AccOk, AccErr ++ [{lists:nth(ProfileIndex, Csv), ProfileType}]}
                end
        end
    end, {[], []}, List),

    ?ASSERT(
        Errors == [],
        itx:format("User for ~p not found: ~p", [ProfileType, Errors])    
    ).


% 
% Validate exam exists
% 
handle_import_validate_exams_exist(List) ->

    ANPTestCourseIDs = lists:map(fun(Csv) ->
        lists:nth(?INDEX_TEST_CODE, Csv)
    end, List),

    SeasonID = minijobcontext:q(import_season_fk),
    Docs = ep_osm_exam_api:getdocs_by_season_fk_anptestcourseids(SeasonID, ANPTestCourseIDs),
    Key = fun(Doc) -> {
        itf:val(Doc, anptestcourseid), itf:val(Doc, startdate)
    } end,

    DocsDict = helper:get_dict_from_docs(Docs, Key),

    {_, Errors} = lists:foldl(fun([TestId, Date | _], {Accoks, AccErr}) ->
        case dict:find({TestId, Date}, DocsDict) of
            {ok, Doc} ->
                {Accoks ++ [Doc], AccErr};
            _ ->
                {Accoks, AccErr ++ [{TestId, Date}]}
        end
    end, {[], []}, List),

    ?ASSERT(
        Errors == [],
        itx:format("Cannot find ~p", [Errors])
    ).

%------------------------------------------------------------------------------
% handle import validate batch
%------------------------------------------------------------------------------

handle_import_validate_batch(List) ->
    ok = handle_validate_batch_candidate_exists(List),
    ok.


%
% handle validate candidate exists
%
handle_validate_batch_candidate_exists(List) ->
    Dict = get_exam_wise_candidate_dict(List),

    CandidateNotFound = lists:map(fun({TestId, DocsToValidate}) ->
        {ok, ExamDoc} = ep_osm_exam_api:get(TestId),
        CandidateDocs = ep_osm_candidate_import:get_existing_candidate_docs(ExamDoc, DocsToValidate),
        CandidateDocsDict = ep_osm_candidate_import:get_existing_candidate_docs_dict(CandidateDocs, undefined),

        %
        % ensure candidates exists
        %
        validate_candidate_exists(DocsToValidate, CandidateDocsDict)
    end, dict:to_list(Dict)),

    ?ASSERT(
        import_validation,
        lists:flatten(CandidateNotFound) == [],
        itx:format("Candidate ~p not found", [CandidateNotFound])
    ).


%
% ensure candidate exists
%
validate_candidate_exists(DocsToValidate, CandidateDocsDict) ->
    lists:foldl(fun(Doc, AccErrors) ->
        case dict:find(itf:val(Doc, anpseatnumber), CandidateDocsDict) of
            {ok, _} -> AccErrors;
            _ -> AccErrors ++ [itf:val(Doc, anpseatnumber)]
        end
    end, [], DocsToValidate).


%
% get test wise candidate dict
%
get_exam_wise_candidate_dict(List) ->
    %
    % get subject dict
    %
    ExamDocsDict = get_anptest_docs_dict(List),
    ProfileDocsDict = get_username_dict_by_usertype(List),

    FsRes = lists:map(fun(Csv) ->
        TestCode = lists:nth(?INDEX_TEST_CODE, Csv),
        ExamDate = lists:nth(?INDEX_DATE, Csv),
        {ok, TestDoc} = dict:find({TestCode, ExamDate}, ExamDocsDict),

        Fs = [
            fields:build(anptestcourseid, lists:nth(?INDEX_TEST_CODE, Csv)),
            fields:build(anpseatnumber, lists:nth(?INDEX_ANP_PRN, Csv))
        ] ++
        get_eval_fs(ProfileDocsDict, anpevaluator, lists:nth(?INDEX_ANP_EVALUATOR, Csv)) ++
        get_eval_fs(ProfileDocsDict, anpmoderator, lists:nth(?INDEX_ANP_MODERATOR, Csv)) ++
        get_eval_fs(ProfileDocsDict, anprevaluator, lists:nth(?INDEX_ANP_REVALUATOR, Csv)) ++
        get_eval_fs(ProfileDocsDict, anpmoderator_reval, lists:nth(?INDEX_ANP_MODERATOR_REVALUATOR, Csv)),

        {itf:idval(TestDoc), Fs}
    end, List),

    lists:foldl(fun({TestId, FsList}, Acc) ->
        dict:append(TestId, helper_api:fields2doc(FsList), Acc)
    end, dict:new(), FsRes).


get_anptest_docs_dict(List) ->

    ANPTestCourseIDs = lists:map(fun(Csv) ->
        lists:nth(?INDEX_TEST_CODE, Csv)
    end, List),

    SeasonID = minijobcontext:q(import_season_fk),
    Docs = ep_osm_exam_api:getdocs_by_season_fk_anptestcourseids(SeasonID, ANPTestCourseIDs),
    Key = fun(Doc) -> {
        itf:val(Doc, anptestcourseid), itf:val(Doc, startdate)
    } end,

    helper:get_dict_from_docs(Docs, Key).


get_username_dict_by_usertype(List) ->
    Usernames = lists:foldl(fun(Csv, Acc) ->
        Acc ++
        [lists:nth(?INDEX_ANP_EVALUATOR, Csv)] ++
        [lists:nth(?INDEX_ANP_MODERATOR, Csv)] ++
        [lists:nth(?INDEX_ANP_REVALUATOR, Csv)] ++
        [lists:nth(?INDEX_ANP_MODERATOR_REVALUATOR, Csv)]
    end, [], List),
    
    Usernames1 = lists:foldl(fun(Username,Acc) ->
        case Username of
            "NA" ->
                Acc;
            _ ->
                Acc ++ [Username]
        end
    end, [], Usernames),

    ProfileDocs = profiles:getdocs_by_usernames(
        helper:unique(Usernames1)
    ),

    Key = fun(Doc) ->
    {itf:val(Doc, username), ?L2A(itf:val(Doc, profiletype))}
    end,
    
    helper:get_dict_from_docs(ProfileDocs, Key).


get_eval_fs(Dict, ProfileType, Username) ->
    ProfileType1 = ?L2A(itx:format("profileidfk_~p", [ProfileType])),
    case dict:find({Username, ProfileType}, Dict) of
        {ok, Doc} ->
            [fields:build(ProfileType1, itf:idval(Doc))];
        _ ->
            []
    end.


% ----------------------------------------------------------------------------
% handle import csv to fs - imports student master data
%------------------------------------------------------------------------------

handle_import_csv_to_fs(List) ->
    get_exam_wise_candidate_dict(List).


handle_merge_with_existing_docs(ExamDoc, DocsToSave, KeyToFind) ->
    %
    % get existing candidate docs
    %
    ExistingCandidateDocs = ep_osm_candidate_import:get_existing_candidate_docs(
        ExamDoc,
        DocsToSave
    ),
    ExistingCandidateDocsDict = ep_osm_candidate_import:get_existing_candidate_docs_dict(
        ExistingCandidateDocs,
        KeyToFind
    ),

    %
    % merge into existing docs
    %
    lists:foldl(fun(Doc, Acc) ->

        SNO = itf:val(Doc, anpseatnumber),

        case dict:find(ep_osm_candidate_import:get_dict_key(SNO, Doc, KeyToFind), ExistingCandidateDocsDict) of

            {ok, DocExisting} ->
                
                FsDocExisting = itf:d2f(DocExisting, ep_osm_candidate_import:fs(all)),
                FsDoc = helper_api:doc2fields({ok, Doc}),
                FsDocMerged = itf:fs_merge(FsDocExisting, FsDoc),

                %
                % acc only if there are changes
                %
                case itf:fs_changelist(FsDocExisting, FsDoc) of
                    [] ->
                        Acc;
                    _ ->
                        Acc ++ [
                            helper_api:fields2doc(FsDocMerged)
                        ]
                end;

            _ ->
                Acc ++ [
                    Doc
                ]
        end
    end, [], DocsToSave).


%------------------------------------------------------------------------------
% db
%------------------------------------------------------------------------------

%
% save
%
savebulk(LoLofFields) ->

    LoLofFields2 = dict:to_list(LoLofFields),

    {Oks, Errors} = lists:foldl(fun({AnpTestId, DocsToSave},{AccOks, AccErrors}) ->

        {ok, ExamDoc} = ep_osm_exam_api:get(AnpTestId),
        DocsToSave1 = handle_merge_with_existing_docs(ExamDoc, DocsToSave, undefined),
        Db = anpcandidate:db(itf:idval(ExamDoc)),
        BatchSize = 100,
        {ResOks, ResErrors} = db:savebulk(Db, DocsToSave1, BatchSize),
        {AccOks ++ [ResOks], AccErrors ++ [ResErrors]}

    end, {[], []}, LoLofFields2),
    {lists:flatten(Oks), lists:flatten(Errors)}.


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------