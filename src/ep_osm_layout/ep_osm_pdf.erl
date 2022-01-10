-module(ep_osm_pdf).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").


%------------------------------------------------------------------------------
% main
%------------------------------------------------------------------------------

layout(TFs, Type) ->

	%
	% init
	%
	DocId = fields:getuivalue(TFs, '_id'),
	AttachmentNames = attachment:get_names(anptests:getdb(), DocId),
	PdfName = itx:format("~s_~p.pdf", [
		fields:getuivalue(TFs, anptestcourseid), Type
	]),


	%
	% layout
	%
	case lists:member(PdfName, AttachmentNames) of
		false ->
			layout_pdf_unavailable(Type);
		_ ->
			layout(
				TFs, DocId, PdfName, Type,
				itxconfigs_cache:get2(ep_osm_pdf_layout, "pdf")
			)
	end.



%..............................................................................
%
% images 
%
%..............................................................................

layout(_TFs, DocId, PdfName, Type, "images") ->

	%
	% init
	%
	{ok, Cwd} = file:get_cwd(),
	Dir = itx:format("~s/scratch/ep_osm_~p/~s/", [
		Cwd, Type, DocId
	]),
	Filepath = itx:format("/scratch/ep_osm_~p/~s", [
		Type, DocId
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
			handle_convert_pdf_to_images(DocId, PdfName, Type, Dir);
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
	EsImages = layout:grow([
		layout:g(2, layout_anchors(Filenames3)),
		layout:g(8, layout_images(Filenames3, Filepath))
	]),


	%
	% layout
	%
	[
		EsImages,
		layout:g(8, 2, layout_recreate_dir(Dir))
	];


%..............................................................................
%
% pdf 
%
%..............................................................................

layout(_TFs, DocId, PdfName, _Type, _) ->
	attachment:save_in_dir("./scratch/", anptests:getdb(), DocId, PdfName),
	Body = "<iframe
		width='100%' height='1000'
		frameborder='0' scrolling='auto'
		src='/scratch/~s#toolbar=0&navpanes=0&scrollbar=0&zoom=100%'>
	</iframe>",
	io_lib:format(Body, [PdfName]).




%------------------------------------------------------------------------------
% handlers
%------------------------------------------------------------------------------

handle_convert_pdf_to_images(DocId, PdfName, Type, Dir) ->

	%
	% save pdf in dir
	%
	attachment:save_in_dir(Dir, anptests:getdb(), DocId, PdfName),


	%
	% conver pdf to images
	%
	CmdRes = helper:cmd("cd ~s; convert -alpha remove -density 150 ~s -quality 100 ~p.jpg", [
		Dir, PdfName, Type
	]),
	?ASSERT(
		CmdRes == [],
		CmdRes
	).


%------------------------------------------------------------------------------
% misc
%------------------------------------------------------------------------------

%
% layout - pdf unavailable
%
layout_pdf_unavailable(Type) ->

	%
	% init
	%
	PaperType = case Type of
		questionpaper ->
			"Question paper";
		modelanswers ->
			"Model answers";
		_ ->
			"Paper"
	end,


	%
	% layout
	%
	#panel {
		class="mywell",
		body=[
			PaperType ++ " is not available for this test"
		]
	}.



%
% layout images
%
layout_images(Filenames, Filepath) ->
	lists:map(fun(Filename) ->
		Url = itx:format("~s/~s", [Filepath, Filename]),
		Es = #panel {style="width: 100%; overflow-x: scroll;", body=[
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
		]},
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



%
% layout recreate dir
%
layout_recreate_dir(Dir) ->
	#button {
		text="Refresh Pages",
		class="btn btn-link pull-sm-right",
		delegate=?MODULE,
		postback={recreate_dir, Dir}
	}.

%------------------------------------------------------------------------------
% events
%------------------------------------------------------------------------------

event({recreate_dir, Dir}) ->
	helper:cmd("rm -rf ~s", [Dir]),
	helper:redirect(wf:uri()).	



%------------------------------------------------------------------------------
% end
%------------------------------------------------------------------------------
