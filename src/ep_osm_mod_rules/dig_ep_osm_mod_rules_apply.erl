
-module(dig_ep_osm_mod_rules_apply).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


-define(BATCH_SIZE, 100).

%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

title() ->
	?LN("Apply Moderation Rules").

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
		module=?MODULE,
		filters=anptest:fs(search)
	}.


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("Apply Moderation Rules").



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
% Fs
%
%..............................................................................
fetch(D, _From, _Size, []) ->
	{D, []};

fetch(D, From, Size, Fs) ->

	%
	% fetch documents from db
	%
	Docs = get_test_docs(Fs, From, Size),


	%
	% layout results
	%
	Results = lists:map(fun(Doc) ->

		%
		% layout cells
		%
		FsDoc = itf:d2f(Doc, anptest:fs(search)),
		FsIndex = itf:d2f(Doc, anptest:fs(index)),
 		[
			#dcell {val=helper_ui:layout_slinks(anptest, FsIndex)}

		] ++ lists:map(fun(F) ->
			#dcell {val=itl:render(F)}
		end, FsDoc)

	end, Docs),


	%
	% header
	%
	Header = [
		#dcell {type=header, val="Actions"}
	] ++ lists:map(fun(#field {label=Label}) ->
		#dcell {type=header, val=Label}
	end, anptest:fs(search)),

	{
		D#dig {
			total=?INFINITY,
			actions=[
				{action_select_moderation_rule, "Select Moderation Rule", "Select Moderation Rule"}
			]
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

event({confirmation_yes, apply}) ->
	handle_apply_yes();

event(apply) ->
	handle_apply();

event(action_select_moderation_rule) ->
	handle_select_moderation_rule();

event({itx, E}) ->
	ite:event(E).



%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - apply yes
%
%..............................................................................

handle_apply_yes() ->

	%
	% init
	%
	itl:modal_close(),
	Context = wf_context:context(),
	D = helper:state(dig),
	Fs = dig:get_nonempty_fs(D#dig.filters),
	Docs = get_test_docs(Fs, 0, ?INFINITY),
	{ok, ModDoc} = ep_osm_mod_rules_api:get(wf:q(osm_mod_rules_fk)),
	Rules = get_moderation_rules(ModDoc),
	RunningMode = wf:q(running_mode),


	%
	% function
	%
	Fun = fun([]) ->
		wf_context:context(Context),
		lists:foreach(fun(Doc) ->
			handle_apply_yes_test_doc(RunningMode, Rules, Doc)
		end, Docs),
		dig:log(success, "Task completed")
	end,


	%
	% add to queue
	%
	taskqueue:create(Fun, []),
	helper_ui:flash(warning, "Added to queue.", 5).



%..............................................................................
%
% handle - apply yes doc
%
%..............................................................................

handle_apply_yes_test_doc(RunningMode, Rules, Doc) ->

	%
	% init
	%
	{ok, Type} = dict:find(type, Rules),
	Testname = io_lib:format("~s ~s", [
		itf:val(Doc, anptestcourseid),
		itf:val(Doc, testname)
	]),
	dig:log(warning, io_lib:format("Processing ~s", [Testname])),


	%
	% apply
	%
	ApplyResDict = handle_apply_yes_test_doc_batch(
		dict:new(), Rules, Doc, 0, get_candidate_docs(Type, Doc, 0, ?BATCH_SIZE)
	),


	%
	% save result of application
	%
	handle_apply_yes_test_save_result(RunningMode, Doc, ApplyResDict, Rules).


%..............................................................................
%
% handle - apply yes test save result
%
%..............................................................................

handle_apply_yes_test_save_result(_RunningMode, _Doc, [], _) ->
	dig:log("Result of apply rule is empty");
handle_apply_yes_test_save_result(RunningMode, Doc, ApplyResDict, Rules) ->


	%
	% init
	%
	TId = itf:idval(Doc),
	ExamDb = anpcandidates:db(TId),
	{ok, Type} = dict:find(type, Rules),
	NewCandidateState = case Type of
		"evaluation" ->
			"anpstate_moderation";
		"moderation" ->
			"anpstate_revaluation";
		"revaluation" ->
			"anpstate_moderation_reval"
	end,



	%
	% save
	%
	lists:foreach(fun({{FromMarks, ToMarks, MovePercentage}, CandidateList}) ->

		%
		% get documents to move
		%
		MoveList = get_x_percent_of(CandidateList, MovePercentage),
		dig:log(info, io_lib:format("[~p to ~p]: ~p % - Moved: ~p", [
			FromMarks, ToMarks, MovePercentage, length(MoveList)
		])),
		dig:log(info, io_lib:format("Moved: ~p", [MoveList])),



		%
		% save
		%
		CandidateDocs = anpcandidates:getdocs_by_snos(TId, MoveList),
		LoLFs = lists:map(fun(CandidateDoc) ->
			Fs = helper_api:doc2fields({ok, CandidateDoc}),
			fields:delete(Fs, anpstate) ++ [
				fields:build(anpstate, NewCandidateState)
			]
		end, CandidateDocs),



		case RunningMode of
			"live_mode" ->
				{ok, SaveRes} = anpcandidates:updateall(ExamDb, LoLFs),
				{Oks, Errors} = db_helper:bulksave_summary(SaveRes),
				dig:log(success, io_lib:format("Oks: ~p, Errors: ~p", [Oks, Errors]));
			"test_mode" ->
				dig:log(danger, "Save skipped in test mode")
		end



	end, dict:to_list(ApplyResDict)).




%..............................................................................
%
% handle - apply yes doc batch
%
%..............................................................................

handle_apply_yes_test_doc_batch(ApplyAcc, _Rules, _ExamDoc, _From, []) ->
	ApplyAcc;
handle_apply_yes_test_doc_batch(ApplyAcc, Rules, ExamDoc, From, CandidateDocs) ->


	%
	% init
	%
	{ok, Type} = dict:find(type, Rules),
	TotalRoleId = case Type of
		"evaluation" ->
			"total_anpevaluator";
		"moderation" ->
			"total_anpmoderator";
		"revaluation"  ->
			"total_anprevaluator"
	end,
	TotalRoleId1 = ?L2A(TotalRoleId),
	dig:log(info, io_lib:format("Batch: ~p", [From])),
	dig:log(info, io_lib:format("Candidate docs: ~p", [length(CandidateDocs)])),

	%
	% apply rule and get updated student docs
	%
	ApplyAcc1 = lists:foldl(fun(CandidateDoc, Acc) ->
		case itf:val(CandidateDoc, TotalRoleId1) of
			[] ->
				Acc;
			Marks ->
				Marks1 = trunc(helper:s2f(Marks)),
				Group = case dict:find(Marks1, Rules) of
					{ok, Group0} ->
						Group0;
					_ ->
						?ASSERT(
							false,
							?FLATTEN(io_lib:format("Marks out of range!  (~p, ~s)", [
								Marks1, itf:val(CandidateDoc, anpseatnumber)
							]))
						)
				end,
				dict:append(Group, itf:val(CandidateDoc, anpseatnumber), Acc)
		end
	end, ApplyAcc, CandidateDocs),



	%
	% apply rules
	%

	From1 = From + ?BATCH_SIZE,
	handle_apply_yes_test_doc_batch(
		ApplyAcc1, Rules, ExamDoc, From1, get_candidate_docs(Type, ExamDoc, From1, ?BATCH_SIZE)
	).


%..............................................................................
%
% handle - apply
%
%..............................................................................

handle_apply() ->

	%
	% init
	%
	D = helper:state(dig),
	Fs = dig:get_nonempty_fs(D#dig.filters),
	Docs = get_test_docs(Fs, 0, ?INFINITY),


	%
	% confirmation
	%
	itl:confirmation(
		#panel {
			class="mycenter",
			body=[
				#p {text="Are you sure you want to apply this rule?"},
				#p {class="text-danger", text=io_lib:format("This rule will be applied to ~p tests", [length(Docs)]) }
			]
		},
		apply
	).


%..............................................................................
%
% handle - select moderation rule
%
%..............................................................................

handle_select_moderation_rule() ->
	Fs = [
		itf:dropdown(?F(running_mode, "Run Mode"), itf:options([
			?F(test_mode, "Test Mode"),
			?F(live_mode, "Live Mode")
		])),
		?OSMRLS(osm_mod_rules_fk)
	],
	Es = itl:get(?CREATE, Fs, ite:get(apply), table),
	dig_mm:handle_show_action("Moderation Rule", Es).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


get_test_docs([], _, _) ->
	[];
get_test_docs(Fs, From, Size) ->
	Rec = db2_find:getrecord_by_fs(anptests:getdb(), Fs, From, Size),
	#db2_find_response {docs=Docs}  = db2_find:find(
		Rec#db2_find {sort=anptest:fs(search)}
	),
	Docs.




get_candidate_docs(Type, ExamDoc, From, Size) ->

	%
	% init
	%
	ExamId = itf:idval(ExamDoc),
	ExamDb = anpcandidates:db(ExamId),

	CandidateState = case Type of
		"evaluation" ->
			"anpstate_completed";
		"moderation" ->
			"anpstate_moderation_completed";
		"revaluation" ->
			"anpstate_revaluation_completed"
	end,

	%
	% exec
	%
	#db2_find_response {docs=Docs}  = db2_find:get_by_fs(
		ExamDb, [fields:build(anpstate, CandidateState)], From, Size
	),
	Docs.





get_moderation_rules(ModDoc) ->

	%
	% init
	%
	Vals = mylist_field:val(itf:d2f(ModDoc, ?OSMRLS(rules))),


	%
	% build dict
	%
	KVList = lists:foldl(fun({_RuleId, RuleVals}, Acc) ->

		%
		% get vals
		%
		FromMarks = proplists:get_value("frommarks", RuleVals),
		ToMarks = proplists:get_value("tomarks", RuleVals),
		MovePercentage = proplists:get_value("movepercentage", RuleVals),
		FromMarksInt = ?S2I(FromMarks),
		ToMarksInt = ?S2I(ToMarks),
		MovePercentageInt = ?S2I(MovePercentage),


		KVList0 = lists:map(fun(I) ->
			{I, {FromMarksInt, ToMarksInt, MovePercentageInt}}
		end, lists:seq(FromMarksInt, ToMarksInt)),


		Acc ++ KVList0


	end, [], Vals),


	%
	% return dict
	%
	KVList1 = KVList ++ [
		{type, itf:val(ModDoc, type)}
	],
	dict:from_list(KVList1).





%
% get x percent from list
%
get_x_percent_of(_CandidateList, 0) ->
	[];
get_x_percent_of(CandidateList, 100) ->
	CandidateList;
get_x_percent_of(CandidateList, MovePercentage) ->
	MoveCount = helper:ceiling(length(CandidateList) * MovePercentage / 100),
	lists:map(fun(_) ->
		Index = rand:uniform(length(CandidateList)),
		lists:nth(Index, CandidateList)
	end, lists:seq(1, MoveCount)).



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
