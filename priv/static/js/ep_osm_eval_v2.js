///////////////////////////////////////////////////////////////////////////////

var ANP = {};

ANP.mapPageNoWithCanvasID = {};
ANP.PageCount = 1;

ANP.BG_WIDTH = 1300;
ANP.BG_HEIGHT = 900;

ANP.canvasobjs = {};
ANP.cursorText = undefined;

ANP.OFFSET_TOP = 0;
ANP.showing_marking_scheme = true;

ANP.IMGURL_CORRECT = 'https://lib.weshinetech.in/images/correct.png';
ANP.IMGURL_WRONG = 'https://lib.weshinetech.in/images/wrong.png';

///////////////////////////////////////////////////////////////////////////////

ANP.scrollposap = 0;
ANP.showpanel = function (panelid, switched, aname) {

	// record ap-panel scroll position
	if ($(".visiblepanel").hasClass("wfid_anpcandidate_answerpaper")) {
		ANP.scrollposap = $(document).scrollTop();
	}

	// hide current panel and show requested panel
	$(".visiblepanel").removeClass("visiblepanel").addClass('hidden');
	$(".wfid_" + panelid).addClass("visiblepanel").removeClass('hidden');

	// if shown panel is anp-panel, then scroll to its previous position
	if (panelid == "anpcandidate_answerpaper") {
		ANP.show_on_screen_widgets();
		if(switched) {
			//
			// switched page number from page nav dropdown in navbar or switched page
			//
			ANP.OFFSET_TOP = $('.navbar.navbar-expand').outerHeight()
			   + $('.sticky-top').outerHeight();
			var aTag = $("div[id='"+ aname +"']");
			$('html,body').animate({scrollTop: aTag.offset().top - ANP.OFFSET_TOP}, 'fast');
		}
		else{
			//
			// coming back from remarks, question or model papers page
			//
			$(document).scrollTop(ANP.scrollposap);

		}
	} else {
		//
		// going to other pages than answer paper
		//
		ANP.hide_on_screen_widgets();
		$(document).scrollTop(0);
	}

};

///////////////////////////////////////////////////////////////////////////////

ANP.layout_answerpaper_page = function (imgurl, canvasdata) {

	// get url of the current image
	var canvasid = imgurl;

	// create a wrapper around native canvas element (with id="c")
	var canvas = new fabric.Canvas(canvasid, {
		isDrawingMode: true
	});


	//
	// if cltr or cltr + alt keys are pressed then
	// add images correct / incorrect images on the canvas
	//
	canvas.on('mouse:down', function(options) {

		if(options.e.ctrlKey) {

			var pointer = canvas.getPointer(options.e);
			var posX = pointer.x;
			var posY = pointer.y;

			var img = new Image();
			img.crossOrigin = "anonymous";
			img.onload = function() {
				canvas.add(new fabric.Image(img, {
					left: posX - 25,
					top: posY -25,
					width: 50,
					height: 50,
				}));
			};

			if(options.e.altKey) {
				img.src = ANP.IMGURL_WRONG;
			} else {
				img.src = ANP.IMGURL_CORRECT;
			}
		}

		if(options.target)
		{
			console.log("a target object was clicked", options.target.type);
		}

	});

	// handle canvas change events
	canvas.on("object:added", function() {
		ANP.save_answerpaper_page(canvasid);
	});

	// handle canvas change events
	canvas.on("object:removed", function(o) {
		ANP.save_answerpaper_page(canvasid);
	});


	// init canvas properties
	canvas.freeDrawingBrush.color = "red";
	canvas.freeDrawingBrush.width = 2;

	// load canvas data
	ANP.setBackgroundImage(canvas, imgurl);

	if (canvasdata != "false") {
		canvas.loadFromJSON(canvasdata);
	}

	// set canvas width and height
	canvas.setWidth(ANP.BG_WIDTH);
	canvas.setHeight(ANP.BG_HEIGHT);

	// store canvas objects
	ANP.canvasobjs[canvasid] = canvas;

	// map page numbers with answer paper image file name for page navigation by pageNo
	ANP.mapPageNoWithCanvasID[ANP.PageCount] = canvasid;
	ANP.PageCount = ANP.PageCount + 1;

};

///////////////////////////////////////////////////////////////////////////////

ANP.save_answerpaper_page = function (canvasid) {
	ANP.mark_page_in_page_nav_dropdown();

	var canvas = ANP.canvasobjs[canvasid];
	if (page && page.canvas_save) {
		page.canvas_save(canvasid, JSON.stringify(canvas));
	}

};

///////////////////////////////////////////////////////////////////////////////

ANP.set_cursor_text = function (cursorText) {
	ANP.cursorText = cursorText;
	$(document).trigger('mousemove');
};

ANP.set_cursor_tooltip = function (e) {
	if (ANP.cursorText) {
		var mouseX = e.clientX - 10;
		var mouseY = e.clientY + 30;
		if ($("#cursor_tooltip").length) {
			$("#cursor_tooltip").html(ANP.cursorText);
		} else {
			$("body").append("<div class='well' id='cursor_tooltip'>" + ANP.cursorText + "</div>");
		}
		$("#cursor_tooltip").css(
			{'top': mouseY, 'left':mouseX, 'position': 'fixed', 'display': 'block'}
		);
	}
};

///////////////////////////////////////////////////////////////////////////////

ANP.clicked_flip = function (canvasid) {
	var canvas = ANP.canvasobjs[canvasid];
	canvas.backgroundImage.flipY = !canvas.backgroundImage.flipY;
	canvas.backgroundImage.flipX = !canvas.backgroundImage.flipX;
	canvas.renderAll();
	ANP.save_answerpaper_page(canvasid);
};

///////////////////////////////////////////////////////////////////////////////

ANP.clicked_rotate = function (canvasid) {
	var canvas = ANP.canvasobjs[canvasid];

	var curAngle = canvas.backgroundImage.getAngle();
	if (curAngle == 360)
		curAngle = 0;

	canvas.backgroundImage.left = 0;
	canvas.backgroundImage.top = 0;
	canvas.backgroundImage.width = ANP.BG_WIDTH;
	canvas.backgroundImage.height = ANP.BG_HEIGHT;
	canvas.backgroundImage.setAngle(curAngle+90);

	var quadrant = (curAngle / 90) % 4;

	if (quadrant == 0) {
		canvas.backgroundImage.left = ANP.BG_WIDTH;
		canvas.backgroundImage.scaleX = ANP.BG_WIDTH / canvas.backgroundImage.height;
		canvas.backgroundImage.scaleY = ANP.BG_WIDTH / canvas.backgroundImage.height;
	} else if (quadrant == 1) {
		canvas.backgroundImage.left = ANP.BG_WIDTH;
		canvas.backgroundImage.top = ANP.BG_HEIGHT;
		canvas.backgroundImage.scaleToWidth(ANP.BG_WIDTH);
		canvas.backgroundImage.scaleToHeight(ANP.BG_HEIGHT);
	} else if (quadrant == 2) {
		canvas.backgroundImage.top = 450;
		canvas.backgroundImage.scaleX = ANP.BG_WIDTH / canvas.backgroundImage.height;
		canvas.backgroundImage.scaleY = ANP.BG_WIDTH / canvas.backgroundImage.height;
	} else if (quadrant == 3) {
		canvas.backgroundImage.scaleToWidth(ANP.BG_WIDTH);
		canvas.backgroundImage.scaleToHeight(ANP.BG_HEIGHT);
	}

	canvas.renderAll();
	ANP.save_answerpaper_page(canvasid);
};

///////////////////////////////////////////////////////////////////////////////

ANP.clicked_clear = function (canvasid) {
	var canvas = ANP.canvasobjs[canvasid];
	canvas.clear().renderAll();
	ANP.setBackgroundImage(canvas, canvasid);
	ANP.save_answerpaper_page(canvasid);

	//
	// remove green button color
	//
	$('.page-nav-dropdown input[type="button"][value=' + ANP.get_page_no() + ']')
	.removeClass('bg-success btn-success btn-outline-success')
	.addClass('btn-outline-secondary');
};

///////////////////////////////////////////////////////////////////////////////

ANP.clicked_undo = function (canvasid) {

	var canvas = ANP.canvasobjs[canvasid];
	var canvas_objects = canvas._objects;
	if(canvas_objects.length !== 0){
		var last = canvas_objects[canvas_objects.length -1];

		canvas.remove(last);
		canvas.renderAll();
	}
};

///////////////////////////////////////////////////////////////////////////////

ANP.setBackgroundImage = function (canvas, imgurl) {

	var img = new Image();
	img.crossOrigin = "anonymous";
	// TODO this sometimes creates a blank canvas image
	img.onload = function() {
		canvas.setBackgroundImage(new fabric.Image(img, {
			originX: 'left',
			originY: 'top',
			left: 0,
			top: 0,
			width: ANP.BG_WIDTH,
			height: ANP.BG_HEIGHT,
		}), canvas.renderAll.bind(canvas));
	};
	img.src = imgurl;
};

///////////////////////////////////////////////////////////////////////////////

ANP.download_pdf = function (sno) {

	var pdf = new jsPDF();
	var width = pdf.internal.pageSize.width;
	var height = pdf.internal.pageSize.height;

	for(var canvasid in ANP.canvasobjs) {
		var canvas = ANP.canvasobjs[canvasid];
		var imgData = canvas.toDataURL("image/jpeg", 1.0);
		pdf.addPage();
		pdf.addImage(imgData, 'JPEG', 0, 0, width, height);
	}
	pdf.save(sno + ".pdf");
};


///////////////////////////////////////////////////////////////////////////////

ANP.mark_page_in_page_nav_dropdown = function() {
	$('.page-nav-dropdown input[type="button"][value=' + ANP.get_page_no() + ']')
		.addClass('btn-success btn-outline-success');
}

///////////////////////////////////////////////////////////////////////////////

ANP.get_page_no = function(){
	ANP.OFFSET_TOP = $('.navbar.navbar-expand').outerHeight()
			   + $('.sticky-top').outerHeight();
	var scrolled = $(document).scrollTop() + ANP.OFFSET_TOP;
	var pageHeight = $('.AnpPage').parent().parent().outerHeight();

	pageNo = 1 + Math.floor(scrolled/pageHeight);
	return pageNo;
}

///////////////////////////////////////////////////////////////////////////////

ANP.get_active_canvasID = function () {

	return ANP.mapPageNoWithCanvasID[ANP.get_page_no()];
}

///////////////////////////////////////////////////////////////////////////////
//
// update page number on navbar
//
///////////////////////////////////////////////////////////////////////////////
ANP.update_page_number_on_navbar = function() {
	ActivePage = ANP.get_page_no();
	if (true == $('.visiblepanel').hasClass("wfid_anpcandidate_answerpaper")) {
		$('#navbar_page_no').text("".concat(ActivePage, "/", ANP.PageCount -1, "  " ));
	}
}


///////////////////////////////////////////////////////////////////////////////
//
// mark current number as blue in page navigation dropdown
//
///////////////////////////////////////////////////////////////////////////////
ANP.mark_current_page_in_page_navigation_dropdown = function(){
	//
	// remove blue color from previous button
	//
	$('.page-nav-dropdown input[type="button"].bg-primary').removeClass(
		'bg-primary btn-primary border-primary');
	//
	// add blue color to current button
	//
	$('.page-nav-dropdown input[type="button"][value=' + ActivePage + ']')
	.addClass('bg-primary btn-primary border-primary');
}

///////////////////////////////////////////////////////////////////////////////
//
// mark current number as blue in page navigation dropdown
//
///////////////////////////////////////////////////////////////////////////////

ANP.highlight_active_page = function() {

	// remove blue border from previous page div
	$('.border-2.rounded-3').removeClass('border border-2 rounded-3 border-primary');

	// add blue border to active page div
	$('.PageNum_'+ ActivePage).parent().addClass('border border-2 rounded-3 border-primary');
}



///////////////////////////////////////////////////////////////////////////////
//
// expand marking scheme on left side hover
//
///////////////////////////////////////////////////////////////////////////////
ANP.expand_marking_scheme_layout = function(event) {
	if(ANP.showing_marking_scheme == false && event.pageX < 20) {
		ANP.showing_marking_scheme = true;
		expand_marks_box();
	} else if(ANP.showing_marking_scheme == true && event.pageX > 250) {
		ANP.showing_marking_scheme = false;
		collapse_marks_box();
	}
}

////////////////////////////////////////////////////////////////////////////////
//
// sticky-top class does not work if any parent element has overflow property
// set
//
////////////////////////////////////////////////////////////////////////////////

ANP.fixForStickyNavbar = function() {
	let parent = document.querySelector('.sticky-top').parentElement;

	while (parent) {
	    const hasOverflow = getComputedStyle(parent).overflow;
	    if (hasOverflow !== 'visible') {
	        parent.style.overflow = "unset";
	    }
	    parent = parent.parentElement;
	}
};


///////////////////////////////////////////////////////////////////////////////
//
// Make DropDown Z-index above sticky navbar
//
////////////////////////////////////////////////////////////////////////////////
ANP.raiseDropDownOverStickyNavbar = function() {
	$('.navbar-collapse').css("zIndex",1021);
};


////////////////////////////////////////////////////////////////////////////////
//
// Hide widgets on remarks panel
//
////////////////////////////////////////////////////////////////////////////////
ANP.show_on_screen_widgets = function() {
	$('#marks_box').css('visibility', 'visible');
	$('#toolbar_floating').css('visibility', 'visible');
};

ANP.hide_on_screen_widgets = function() {
	$('#marks_box').css('visibility', 'hidden');
	$('#toolbar_floating').css('visibility', 'hidden');
};

////////////////////////////////////////////////////////////////////////////////
//
// navbar full screen button
//
////////////////////////////////////////////////////////////////////////////////
ANP.enable_navbar_fullscreen_button = function() {
	var viewFullScreen = document.getElementById("view-fullscreen");

	if (viewFullScreen) {
	  viewFullScreen.addEventListener("click", function() {
			console.log("fullscreen ");

		var isInFullScreen =
			(document.fullscreenElement && document.fullscreenElement !== null) ||
			(document.webkitFullscreenElement && document.webkitFullscreenElement !== null) ||
			(document.mozFullScreenElement && document.mozFullScreenElement !== null) ||
			(document.msFullscreenElement && document.msFullscreenElement !== null);

		var docElm = document.documentElement;
		if (!isInFullScreen) {
			if (docElm.requestFullscreen) {
				docElm.requestFullscreen();
			} else if (docElm.mozRequestFullScreen) {
				docElm.mozRequestFullScreen();
			} else if (docElm.webkitRequestFullScreen) {
				docElm.webkitRequestFullScreen();
			} else if (docElm.msRequestFullscreen) {
				docElm.msRequestFullscreen();
			}
		} else {
			if (document.exitFullscreen) {
				document.exitFullscreen();
			} else if (document.webkitExitFullscreen) {
				document.webkitExitFullscreen();
			} else if (document.mozCancelFullScreen) {
				document.mozCancelFullScreen();
			} else if (document.msExitFullscreen) {
				document.msExitFullscreen();
			}
		}

	  });
	}
}


///////////////////////////////////////////////////////////////////////////////
//
// TIMER for elapsed time since started evaluation
//
///////////////////////////////////////////////////////////////////////////////
WstTimer = {};
WstTimer.secondselapsed = 0;

//
// start timer based on value saved on server
//
WstTimer.start = function (seconds) {
	WstTimer.TimerId = window.setInterval(WstTimer.onTimeOut, 1000);
	WstTimer.secondselapsed = seconds;
};

//
// update UI timer every second
//
WstTimer.onTimeOut = function () {
	var SecondsToAdd = 1;
	WstTimer.secondselapsed = WstTimer.secondselapsed + SecondsToAdd;

	//
	// store time elapsed on server, every half minute
	//
	if ((WstTimer.secondselapsed % 30 ==0)) {
		page.timer_event(WstTimer.secondselapsed);
	};

	WstTimer.updateElement();
};

//
// update UI timer
//
WstTimer.updateElement = function () {
	var mins = Math.floor(WstTimer.secondselapsed / 60);
	var secs = WstTimer.secondselapsed % 60;

	var html = "<span id='time_spent'>" + mins + "m " + secs + "s" + "</span>";

	$("#time_spent").html(html);
};

WstTimer.unload = function () {
	window.clearInterval(WstTimer.TimerId);
	page.unload(WstTimer.secondselapsed);
};


//
// saves timer seconds on page reload, close
//
window.addEventListener('beforeunload', (event) => {
	page.timer_event(WstTimer.secondselapsed);
});


///////////////////////////////////////////////////////////////////////////////
//
// One time necessary init functions
//
///////////////////////////////////////////////////////////////////////////////
$(document).ready(function() {
	ANP.fixForStickyNavbar();
	ANP.raiseDropDownOverStickyNavbar();
	ANP.update_page_number_on_navbar();
	ANP.enable_navbar_fullscreen_button();

	//
	// remove unnecessary padding from review area
	//
	$("main").removeClass("px-3 py-4");
	$(".container-fluid").css('margin', '0');
	$(".container-fluid").css('padding', '0');

	// fix for nagging vertical scrollbar on remaining pages panel
	$('body').css('overflow', 'scroll');

	// disable pesky sidebar which causes cursor offset issues on canvas
	$('#sidebar').toggle();


///////////////////////////////////////////////////////////////////////////////
//
// FLOATING TOOLBAR BUTTONS CLICK EVENT LISTENERS
//
///////////////////////////////////////////////////////////////////////////////
	$('#toolbar_flip').click(function() {
		ANP.clicked_flip(ANP.get_active_canvasID());
	});

	$('#toolbar_rotate').click(function() {
		ANP.clicked_rotate(ANP.get_active_canvasID());
	});

	$('#toolbar_eraseall').click(function() {
		ANP.clicked_clear(ANP.get_active_canvasID());
	});

	$('#toolbar_undo').click(function() {
		ANP.clicked_undo(ANP.get_active_canvasID());
	});


///////////////////////////////////////////////////////////////////////////////
//
// ON SCREEN WIDGETS
//
///////////////////////////////////////////////////////////////////////////////
var marks_box = document.getElementById("marks_box");
	if(marks_box) {
		marks_box.onmousedown = expand_marks_box;
		marks_box.onmouseout = collapse_marks_box;
		marks_box.onmouseover = expand_marks_box;
	}
});

function expand_marks_box() {
	$('#marks_box_mscheme').removeClass('hidden');
}

function collapse_marks_box() {
	$('#marks_box_mscheme').addClass('hidden');
}



///////////////////////////////////////////////////////////////////////////////
//
// MOUSE RELATED FUNCTIONS
//
///////////////////////////////////////////////////////////////////////////////
$(document).mousemove(function(event) {
	ANP.set_cursor_tooltip(event);
	ANP.expand_marking_scheme_layout(event);
});

$(document).scroll(function(event) {
		ANP.update_page_number_on_navbar();
		ANP.mark_current_page_in_page_navigation_dropdown();
		ANP.highlight_active_page();

} );



///////////////////////////////////////////////////////////////////////////////
//
// disable right click context menu
//
///////////////////////////////////////////////////////////////////////////////
document.oncontextmenu = RightMouseDown;
document.onmousedown = mouseDown;
function mouseDown(e) {
	if (e.which==3) {
		return false;
	}
}

function RightMouseDown() {
	return false;
}



///////////////////////////////////////////////////////////////////////////////
//
// keyboard shortcuts
//
///////////////////////////////////////////////////////////////////////////////
document.addEventListener('keydown', function(event) {
  // undo
  if (event.ctrlKey && event.key === 'z') {
    ANP.clicked_undo(ANP.get_active_canvasID());
  }

  //
  // in case page doesn't get loaded correctly, then reload background image and canvasdata
  //
  if (event.ctrlKey && event.key === 'i') {
  	var canvasid = ANP.get_active_canvasID();
    var canvas = ANP.canvasobjs[canvasid];
    ANP.setBackgroundImage(canvas, canvasid);
	canvas.renderAll();
  }

});

///////////////////////////////////////////////////////////////////////////////
