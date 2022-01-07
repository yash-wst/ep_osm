-module(ep_osm_questionpaper).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

layout(TFs) ->

	%
	% init
	%
	DocId = fields:getuivalue(TFs, '_id'),
	AttachmentNames = attachment:get_names(anptests:getdb(), DocId),
	PaperName = fields:getuivalue(TFs, anptestcourseid) ++ "_questionpaper.pdf",


	%
	% layout
	%
	case lists:member(PaperName, AttachmentNames) of
		false ->
			layout_questionpaper_unavailable();
		_ ->
			layout(
				TFs, DocId, PaperName,
				itxconfigs_cache:get2(ep_osm_questionpaper_layout, "pdf")
			)
	end.



%..............................................................................
%
% images 
%
%..............................................................................

layout(_TFs, DocId, PaperName, "images") ->

	%
	% init
	%
	{ok, Cwd} = file:get_cwd(),
	Dir = itx:format("~s/scratch/ep_osm_questionpaper/~s/", [
		Cwd, DocId
	]),
	Filepath = itx:format("/scratch/ep_osm_questionpaper/~s", [
		DocId
	]),



	%
	% create dir
	%
	helper:cmd("mkdir -p ~s", [Dir]),



	%
	% check if images already exist
	% if not, create images from pdf
	%
	{ok, Filenames} = file:list_dir(Dir),
	case Filenames of
		[] ->
			handle_convert_pdf_to_images(DocId, PaperName, Dir);
		_ ->
			skip
	end,
	{ok, Filenames1} = file:list_dir(Dir),
	



	%
	% filter out non jpg files
	%
	Filenames2 = lists:filter(fun(Filename) ->
		filename:extension(Filename) == ".jpg"
	end, Filenames1),
	Filenames3 = helper:sortasc(Filenames2),

	%
	% layout images
	%
	[
		layout:g(2, layout_anchors(Filenames3)),
		layout:g(8, layout_images(Filenames3, Filepath))
	];


%..............................................................................
%
% pdf 
%
%..............................................................................

layout(_TFs, DocId, PaperName, _) ->
	attachment:save_in_dir("./scratch/", anptests:getdb(), DocId, PaperName),
	Body = "<iframe
		width='100%' height='1000'
		frameborder='0' scrolling='auto'
		src='/scratch/~s#toolbar=0&navpanes=0&scrollbar=0&zoom=100%'>
	</iframe>",
	io_lib:format(Body, [PaperName]).




%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------

handle_convert_pdf_to_images(DocId, PaperName, Dir) ->

	%
	% save pdf in dir
	%
	attachment:save_in_dir(Dir, anptests:getdb(), DocId, PaperName),


	%
	% conver pdf to images
	%
	CmdRes = helper:cmd("cd ~s; convert -alpha remove -density 150 ~s -quality 100 Page.jpg", [
		Dir, PaperName
	]),
	?ASSERT(
		CmdRes == [],
		CmdRes
	).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% layout - question paper unavailable
%
layout_questionpaper_unavailable() ->
	#panel {
		class="mywell",
		body=[
			"Question paper is not available for this test"
		]
	}.



%
% layout images
%
layout_images(Filenames, Filepath) ->
	lists:map(fun(Filename) ->
		Url = itx:format("~s/~s", [Filepath, Filename]),
		Es = [
			#p {
				class="bg-info m-5 p-5",
				text=filename:rootname(Filename)
			},
			itx:format("<a id='page~s' href='#'></a>", [
				Filename
			]),
			itx:format("<img src='~s' width='100%' draggable='false'>", [
				Url
			])
		],
		itl:section(Es)
	end, Filenames).



%
% layout anchors
%
layout_anchors(Filenames) ->
	{Es, _} = lists:foldl(fun(Filename, {Acc, Index}) ->
		Link = #link {
			text=itx:format("Page ~p", [Index]),
			url="#page" ++ Filename
		},
		{
			Acc ++ [
				#p {
					body=Link
				}
			],
			Index+1
		}
	end, {[], 1}, Filenames),
	#panel {
		class="pull-sm-right",
		body=[
			#p {class="font-weight-bold", text="Pages"},
			Es
		]
	}.

%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
