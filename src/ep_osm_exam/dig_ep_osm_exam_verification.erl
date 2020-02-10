
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
	ita:auth(?APPOSM, ?MODULE, #template {file="lib/itx/priv/static/templates/html/entered_nomenu.html"}).

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
		filters=[
			?COREXS(season_fk),
			?CORFAC(faculty_code_fk),
			?CORPGM(program_code_fk),
			?CORSUB(subject_code_fk),
			fields:get(anptestcourseid),
			fields:get(teststatus),
			fields:get(exam_pattern),
			itf:build(itf:hidden(osm_exam_fk), wf:q(id)),
			itf:build(itf:hidden(osm_bundle_fk), wf:q(bundleid))
		],
		size=25
	}.


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
	{D, []};



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
	Stats = ep_osm_exam_api:get_bundle_evaluation_stats(ExamId),
	StatsDict = dict:from_list(Stats),


	%
	% get bundle ids
	%
	BundleIds = lists:map(fun({[BundleId, _State], _}) ->
		BundleId
	end, Stats),
	BundleIdsUnique = helper:unique(BundleIds),



	%
	% layout results
	%
	Results = lists:map(fun(BundleId) ->

		%
		% get stats per bundle
		%
		[
			#dcell {val=#link {
				new=true,
				text=BundleId,
				url=io_lib:format("/~p?id=~s&bundleid=~s", [
					?MODULE, ExamId, BundleId
				])
			}}
		] ++ lists:map(fun(State) ->
			Val = case dict:find([BundleId, State], StatsDict) of
				{ok, Count} ->
					Count;
				error ->
					0
			end,
			#dcell {
				bgcolor=get_class(State, Val),
				val=Val
			}
		end, states())

	end, BundleIdsUnique),



	%
	% header
	%
	Header = [
		#dcell {type=header, val="Bundle"}
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
			total=length(BundleIdsUnique),
			description=itf:val(TFs, testname)
		},
		[Header] ++ dig:append_total_cells(ResultsSorted)
	};




%..............................................................................
%
% [osm_exam_fk, osm_bundle_fk]
%
%..............................................................................

fetch(D, _From, _Size, [
	#field {id=osm_exam_fk, uivalue=ExamId},
	#field {id=osm_bundle_fk, uivalue=BundleId}
]) ->

	%
	% init
	%
	ExamDb = anpcandidates:db(ExamId),
	{ok, ExamDoc} = ep_osm_exam_api:get(ExamId),


	%
	% get student docs from osm exam db with the specified bundle id
	%
	FsToSearchBundle = [
		% itf:build(itf:textbox(?F(osm_bundle_fk)), BundleId)
	],
	#db2_find_response {docs=CandidateDocs} = db2_find:get_by_fs(
		ExamDb, FsToSearchBundle, 0, ?INFINITY
	),



	%
	% results
	%
	Results = lists:map(fun(CDoc) ->
		[
			#dcell {val=itf:val(CDoc, anp_paper_uid)},
			#dcell {val=itf:val(CDoc, anpseatnumber)},
			#dcell {val=?LN(?L2A(itf:val(CDoc, anpstate)))},
			#dcell {
				val=#link {
					text="View",
					new=true,
					url=io_lib:format("/ep_osm_eval_view?mode=view&anpid=~s&anptestid=~s&role=anpevaluator", [
						itf:idval(CDoc), ExamId
					])
				}
			}
			% #dcell {
			% 	val=ite:button(
			% 		view_scanned_images,
			% 		"View Images",
			% 		{view_scanned_images, ExamId, itf:idval(CDoc)}
			% 	)
			% }
		]
	end, CandidateDocs),



	%
	% header
	%
	Header = [
		#dcell {type=header, val="Barcode / UID"},
		#dcell {type=header, val="Seat No."},
		#dcell {type=header, val="State"},
		#dcell {type=header, val="Scanned Images"}
	],


	%
	% return
	%
	{D#dig {
		total=length(CandidateDocs),
		description=io_lib:format("~s / ~s / ~s", [
			BundleId,
			itf:val(ExamDoc, testname),
			?LN(?L2A(itf:val(ExamDoc, teststatus)))
		])
	}, [Header] ++ Results};





%..............................................................................
%
% _
%
%..............................................................................

fetch(D, From, Size, Fs) ->
	dig_ep_osm_exam_evaluation_stats:fetch(D, From, Size, Fs).


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



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------