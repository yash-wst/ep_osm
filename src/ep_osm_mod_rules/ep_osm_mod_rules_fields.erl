-module(ep_osm_mod_rules_fields).
-compile(export_all).
-include("records.hrl").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------
f(name = I) ->
	itf:textbox(?F(I, "Name"));


f(type = I) ->
	itf:dropdown(?F(I, "Type"), options(I));


f(min_required_candidates = I) ->
	F = itf:textbox(?F(I, "Minimum candidates")),
	F#field {
		validators=[],
		label_short_text="Minimum number of candidates required in the test for this rule"
	};


f(frommarks = I) ->
	itf:textbox_int(?F(I, "From Marks"));


f(tomarks = I) ->
	itf:textbox_int(?F(I, "To Marks"));


f(movepercentage = I) ->
	itf:textbox_int(?F(I, "Move Percentage"));


f(evaluator_role = I) ->
	itf:dropdown(?F(I, "Role"), options(I));


f(evaluator_role_1 = I) ->
	F = f(evaluator_role),
	F#field {label="Role 1", id=I};


f(evaluator_role_2 = I) ->
	F = f(evaluator_role),
	F#field {label="Role 2", id=I};


f(diffpercentage = I) ->
	itf:textbox_int(?F(I, "Difference (Marks)"));

f(evaluator_role_3 = I) ->
	F = f(evaluator_role),
	F#field {label="Move To", id=I};


f(rules) ->
	Doc = {[]},
	f({rules, Doc});

f({rules = I, Doc}) ->


	%
	% subfield ids
	%
	SubfieldIds = case itf:val(Doc, type) of
		Type when
		Type == "multi_evaluation_difference";
		Type == "difference" -> [
			evaluator_role_1, evaluator_role_2, diffpercentage, evaluator_role_3
		];
		_ ->  [
			frommarks, tomarks, movepercentage
		]
	end,



	%
	% define field
	%
	F = itf:field_group_list(
		{I, ?LN("Moderation Rules")},
		SubfieldIds,
		{
			ep_osm_mod_rules,
			ep_osm_mod_rules_api,
			ep_osm_mod_rules_fields
		}
	),


	%
	% set renderer
	%
	F#field{
		renderer=renderer_subfields_table:get()
	};


f(osm_mod_rules_fk = I) ->
	F = itf:textbox_picker(?F(I, "Moderation Rule")),
	F#field {
		module=ep_osm_mod_rules,
		options=options(I)
	};



f({FId, I}) ->
	?OSMRLS(FId, #field {id=I});


f(O) ->
	throw(O).

%------------------------------------------------------------------------------
% validators
%------------------------------------------------------------------------------
validator(O) ->
	throw(O).

%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------

options(evaluator_role) ->
	itf:options([
		?F(anpevaluator, "Evaluator"),
		?F(anpmoderator, "Moderator"),
		?F(anprevaluator, "Revaluator"),
		?F(anpmoderator_reval, "Reval Moderator"),
		?F(dtp_marks_manual, "DTP Marks (Manual)"),
		?F(dtp_marks_omr, "DTP Marks (OMR)")
	]);

options(osm_mod_rules_fk) ->
	#search {
		title=?LN("Select Moderation Rule"),
		db=ep_osm_mod_rules_api:db(),
		displayfs=ep_osm_mod_rules:fs(grid),
		filterfs=[
			?OSMRLS(name)
		],
		size=10
	};


options(type) ->
	itf:options([
		?F(evaluation, "Apply to evaluators"),
		?F(moderation, "Apply to moderators"),
		?F(revaluation, "Apply to revaluators"),
		?F(difference, "Difference"),
		?F(multi_evaluation_difference, "Difference (Multiple Evalution)")
	]).

%------------------------------------------------------------------------------
% renderers
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
