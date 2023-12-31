-module (ep_osm_photocopy_for_frp).
-compile(export_all).
-include("records.hrl").


photocopy_url(ExamSeasonId, SubjectCode, SubjectPattern, PRN) ->
	photocopy_url(ExamSeasonId, SubjectCode, SubjectPattern, PRN, false).

photocopy_url(ExamSeasonId, SubjectCode, SubjectPattern, PRN, ForceRegenerate) ->

	wf_context:init_context(undefined),

	Bucket = helper_s3:aws_s3_bucket(),
	Region = configs:get(aws_s3_region, "s3.ap-south-1.amazonaws.com"),

	TestDocs = get_tests(ExamSeasonId, SubjectCode, SubjectPattern),

	DownloadUrl = lists:foldl(fun
		(TestDoc, undefined) ->
			try
				S3Dir = itf:val(TestDoc, aws_s3_dir),
				ExpectDir = lists:flatten(S3Dir ++ "/" ++ PRN ++ "/"),

				TId = itf:idval(TestDoc),
				AnpId = get_anpid(PRN, TId),

				Key = build_reval_file_s3_key(ExpectDir, AnpId),

				case {is_exist(Bucket, ExpectDir, AnpId), ForceRegenerate} of
					{true, false} ->
						skip;
					{_, _} ->
						anpcandidate:create_anp_pdf(TId, AnpId, PRN, anpevaluator),
						FPath = "/tmp/" ++ AnpId ++ ".pdf",
						upload_to_s3(FPath, Key, Bucket)
				end,
				download_url(Bucket, Region, Key)
			catch
				_E:_M -> undefined
			end;

		(_, Url) ->
			Url

	end, undefined, TestDocs),

	?ASSERT(DownloadUrl =/= undefined, "ANP candidate not/multiple found for PRN:" ++PRN),

	DownloadUrl.


upload_to_s3(FilePath, Key, BucketName) ->
	AmzResponse = helper_s3:upload_file(BucketName, Key, FilePath),
	AmzRequestId = proplists:get_value("x-amz-request-id", AmzResponse),
	?ASSERT(
			((AmzRequestId /= []) and (AmzRequestId /= undefined)),
			lists:flatten(io_lib:format("S3 upload failed: ~s", [FilePath]))
	).

download_url(BucketName, Region, Key) ->
	lists:flatten(io_lib:format("https://~s.~s/~s", [BucketName, Region, Key])).


build_reval_file_s3_key(ExpectDir, AnpId) ->
	ExpectDir ++ AnpId ++ ".pdf".


get_anpid(PRN, OsmExamId) ->

	ExamDb = anpcandidates:db(OsmExamId),
	FsToSearch = [
		itf:build(itf:textbox(?F(anpseatnumber)), PRN)
	],
	#db2_find_response {docs=OsmCandidateDocs} = db2_find:get_by_fs(ExamDb, FsToSearch, 0, ?INFINITY),

	?ASSERT(length(OsmCandidateDocs) == 1, "ANP candidate not/multiple found for PRN:" ++PRN),

	[AnpCandidate] = OsmCandidateDocs,

	itf:idval(AnpCandidate).


get_tests(ExamSeasonId, SubjectCode, _SubjectPattern) ->

	FsFind = [
		fields:build(season_fk, ExamSeasonId),
		fields:build(anptestcourseid, SubjectCode)
	],

	#db2_find_response {docs=OsmExamDocs0} = db2_find:get_by_fs(anptests:getdb(), FsFind, 0, ?INFINITY),

	lists:filter(fun (TDoc) ->
		itf:val(TDoc, teststatus) == ?COMPLETED
	end, OsmExamDocs0).


is_exist(Bucket, ExpectDir, AnpId) ->
	Files = helper_s3:list_keys(Bucket, ExpectDir),
	lists:foldl(fun
		(_L, true) ->
			true;
		(L, _) ->
			Key = proplists:get_value(key, L),
			Key == ExpectDir++AnpId++".pdf"
	end, false, Files).


