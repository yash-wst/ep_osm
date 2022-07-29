
-module(dig_ep_osm_exam_verification).
-compile(export_all).
-import(dig_ep_osm_exam_evaluation_stats, [
	states/0,
	get_class/2
]).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("itx/include/records_dev.hrl").



%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

main() ->
	main(wf:q(anptestid)).


main(Id) when Id /= undefined ->
	Url = itx:format("/~p?id=~s", [?MODULE, Id]),
	helper:redirect(Url);
main(_) ->
	ita:auth(?APPOSM, ?MODULE, ?AKIT(#template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"})).

title() ->
	?LN("OSM Exam Verification").

heading() ->
	title().


%------------------------------------------------------------------------------
% records
%------------------------------------------------------------------------------



%------------------------------------------------------------------------------
% access
%------------------------------------------------------------------------------
access(_, ?APPOSM_ADMIN) -> true;
access(_, ?APPOSM_CONTROLLER) -> true;
access(_, _) -> false.



%------------------------------------------------------------------------------
% function - get
%------------------------------------------------------------------------------

get() ->
	#dig {
		description="OSM Exam Verification",
		module=?MODULE,
		filters=filters(wf:q(id)),
		size=25
	}.



%------------------------------------------------------------------------------
% filters
%------------------------------------------------------------------------------

filters(ExamId) when ExamId /= undefined ->
	[
		itf:build(itf:hidden(osm_exam_fk), ExamId),
		fields:build(anpcentercode, wf:q(centreid)),
		fields:build(anpstate, wf:q(state)),
		fields:get(anpseatnumber)
	] ++ get_evaluator_filter() ;
filters(_) ->
	[
		?COREXS(season_fk),
		?CORFAC(faculty_code_fk),
		?CORPGM(program_code_fk),
		?CORSUB(subject_code_fk),
		fields:get(anptestcourseid),
		fields:get(teststatus),
		fields:get(exam_pattern),
		itf:build(itf:hidden(osm_exam_fk), wf:q(id))
	].


%------------------------------------------------------------------------------
% function - title
%------------------------------------------------------------------------------
digtitle() ->
	?LN("OSM Exam Verification").



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
fetch(D, _From, _Size, []) ->
	{D, [{custom, #p {text="Please select a filter and search"}}]};



%..............................................................................
%
% [osm_exam_fk]
%
%..............................................................................
fetch(D, _From, _Size, [
	#field {id=osm_exam_fk, uivalue=ExamId}
	]) ->

	%
	% init
	%
	TFs = anptests:get(ExamId),


	%
	% get evaluation stats
	%
	Stats = ep_osm_exam_api:get_centre_evaluation_stats(ExamId),
	StatsDict = dict:from_list(Stats),


	%
	% get centre ids
	%
	CentreIds = lists:map(fun({[CentreId, _State], _}) ->
		CentreId
	end, Stats),
	CentreIdsUnique = helper:unique(CentreIds),



	%
	% layout results
	%
	Results = lists:map(fun(CentreId) ->

		%
		% get stats per bundle
		%
		[
			#dcell {val=CentreId}
		] ++ lists:map(fun(State) ->
			Val = case dict:find([CentreId, State], StatsDict) of
				{ok, Count} ->
					Count;
				error ->
					0
			end,
			#dcell {
				bgcolor=get_class(State, Val),
				val=Val
			}
		end, states()) ++ [
			#dcell {val=#link {
				new=true,
				text="View",
				url=io_lib:format("/~p?id=~s&centreid=~s", [
					?MODULE, ExamId, CentreId
				])
			}}
		]

	end, CentreIdsUnique),



	%
	% header
	%
	Header = [
		#dcell {type=header, val="Centre Code"}
	] ++ lists:map(fun(State) ->
		#dcell {type=header, val=?LN(?L2A(State++"_min"))}
	end, states()) ++ [
		#dcell {type=header, val="Total"}
	],


	%
	% sort results
	%
	ResultsSorted = lists:sort(fun(A, B) ->
		#dcell {val=YetToStartA} = lists:nth(5, A),
		#dcell {val=YetToStartB} = lists:nth(5, B),
		YetToStartA > YetToStartB
	end, Results),



	%
	% return
	%
	{
		D#dig {
			total=length(CentreIdsUnique),
			description=#link {
				url=itx:format("/anptest?mode=view&anptestid=~s", [ExamId]),
				text=io_lib:format("~s / ~s / ~s", [
					itf:val(TFs, anptestcourseid),
					itf:val(TFs, testname),
					?LN(?L2A(itf:val(TFs, teststatus)))
				])
			}
		},
		[Header] ++ dig:append_total_cells(ResultsSorted)
	};


%..............................................................................
%
% [osm_exam_fk]
% plus more filters incoming
%..............................................................................

fetch(D, From, Size, [
	#field {id=osm_exam_fk, uivalue=ExamId} | Fs
]) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(ExamId),
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),


	%
	% get student docs from osm exam db with the specified bundle id
	%
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, Fs, From, Size
	),



	%
	% results
	%
	Results = lists:map(fun(CDoc) ->
		[
			#dcell {val=itf:val(CDoc, anp_paper_uid)},
			#dcell {val=itf:val(CDoc, anpseatnumber)},
			#dcell {val=itf:val(CDoc, anpfullname)},
			#dcell {val=?LN(?L2A(itf:val(CDoc, anpstate)))},
			#dcell {
				val=#link {
					text="View",
					new=true,
					url=io_lib:format("/ep_osm_candidate?mode=view&anpid=~s&anptestid=~s", [
						itf:idval(CDoc), ExamId
					])
				}
			},
			#dcell {
				val=#link {
					text="View",
					new=true,
					url=io_lib:format("/ep_osm_eval_view?mode=view&anpid=~s&anptestid=~s&role=anpevaluator", [
						itf:idval(CDoc), ExamId
					])
				}
			}
		]
	end, CandidateDocs),



	%
	% header
	%
	Header = [
		#dcell {type=header, val="Barcode / UID"},
		#dcell {type=header, val="Seat No."},
		#dcell {type=header, val="Full name"},
		#dcell {type=header, val="State"},
		#dcell {type=header, val="Candidate Doc"},
		#dcell {type=header, val="Scanned Images"}
	],


	%
	% return
	%
	{D#dig {
		total=?INFINITY,
		description=io_lib:format("~s / ~s", [
			itf:val(ExamDoc, testname),
			?LN(?L2A(itf:val(ExamDoc, teststatus)))
		])
	}, [Header] ++ Results}.





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

event({view_scanned_images, ExamId, CandidateId}) ->
	handle_view_scanned_images(ExamId, CandidateId);

event({itx, E}) ->
	ite:event(E).


%------------------------------------------------------------------------------
% handler
%------------------------------------------------------------------------------


%..............................................................................
%
% handle - view scanned images
%
%..............................................................................

handle_view_scanned_images(ExamId, CandidateId) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(ExamId),
	TFs = anptests:get(ExamId),
	Fs = anpcandidates:get(ExamDb, CandidateId),


	%
	% get image urls
	%
	ImageUrls = anpcandidate:get_image_urls(
		fields:getuivalue(TFs, aws_s3_dir), TFs, Fs
	),


	%
	% load images in modal
	%
	?D(ImageUrls).



%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% get additional filter for evaluator
%,
get_evaluator_filter()->
	get_evaluator_filter(wf:q(profiletype), wf:q(profileid)).

get_evaluator_filter(undefined, _) -> [];
get_evaluator_filter( _, undefined) -> [];
get_evaluator_filter( _, "unassigned") -> [];
get_evaluator_filter(ProfileType, ProfileId) ->
	[itf:build(fields:get(?L2A(ProfileType)),ProfileId)].



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
