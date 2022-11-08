-module(ep_osm_candidate_fields).
-compile(export_all).
-include("records.hrl").


%------------------------------------------------------------------------------
% fields
%------------------------------------------------------------------------------
f(anpcandidate_onhold_reasons = I) ->
	itf:multiselect(I, "List of On Hold Reasons", anpcandidate_onhold_reasons_options());

f(O) -> throw(O).

%------------------------------------------------------------------------------
% validators
%------------------------------------------------------------------------------
validator(O) ->
	throw(O).

%------------------------------------------------------------------------------
% options
%------------------------------------------------------------------------------

%
% multiselect options for on hold reasons
%
anpcandidate_onhold_reasons_options() ->
	[
		?F(inward_wrong, locale:get(inward_wrong)),
		?F(subject_code_wrong, locale:get(subject_code_wrong)),
		?F(wrong_paper_scan, locale:get(wrong_paper_scan)),
		?F(double_page_scan, locale:get(double_page_scan)),
		?F(pages_blur, locale:get(pages_blur)),
		?F(pages_cut, locale:get(pages_cut)),
		?F(page_sequence_wrong, locale:get(page_sequence_wrong)),
		?F(page_missing, locale:get(page_missing)),
		?F(masking_missing, locale:get(masking_missing))
	].
%------------------------------------------------------------------------------
% renderers
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------

