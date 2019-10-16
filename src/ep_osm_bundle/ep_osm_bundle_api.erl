
-module(ep_osm_bundle_api).
-compile(export_all).
-include("records.hrl").

db() ->
	"ep_osm_bundle".

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
% create
%------------------------------------------------------------------------------
create(Fs) ->
	%
	% execute create in queue to avoid duplicating bundle numbers
	%
	mini_task_queue:now(fun create1/1, Fs).


create1(Fs) ->

	%
	% init
	%
	ExamId = itf:val2(Fs, osm_exam_fk),
	FOsmExam = itf:build(?OSMBDL(osm_exam_fk), ExamId),


	%
	% get next bundle number
	%
	#db2_find_response {docs=Docs} = db2_find:get_by_fs(
		db(), [FOsmExam], 0, ?INFINITY
	),
	NextBundleNumber = length(Docs) + 1,


	%
	% merge fields
	%
	FsCreate = Fs ++ [
		itf:build(?OSMBDL(number), ?I2S(NextBundleNumber))
	],



	%
	% save
	%
	?MODULE:save(FsCreate).



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
	Docs = lists:map(fun(Fs) ->
		helper_api:fields2doc(Fs)
	end, LoLofFields),

	db:savebulk(db(), Docs).


%------------------------------------------------------------------------------
% mutate
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% delete
%------------------------------------------------------------------------------
delete_by_field(F = #field {}) ->
	{_, Docs} = get_by_field(F),
	lists:map(fun(D) ->
		db:delete_id(db(), itf:idval(D))
	end, Docs).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------


%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
