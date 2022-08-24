///////////////////////////////////////////////////////////////////////////////

var ANP = {};

ANP.mapPageNoWithCanvasID = {};
ANP.PageCount = 1;

ANP.BG_WIDTH = 1300;
ANP.BG_HEIGHT = 900;

ANP.canvasobjs = {};
ANP.cursorText = undefined;

ANP.OFFSET_TOP = 40;

ANP.showing_marking_scheme = true;

ANP.IMGURL_CORRECT = 'https://lib.weshinetech.in/images/correct.png';
ANP.IMGURL_WRONG = 'https://lib.weshinetech.in/images/wrong.png';

///////////////////////////////////////////////////////////////////////////////

ANP.scrollposap = 0;
ANP.showpanel = function (panelid, switched, aname) {

	// record ap-panel scroll position
	if ($(".VISIBLEPANEL").hasClass("wfid_anpcandidate_answerpaper")) {
		ANP.scrollposap = $(document).scrollTop();
	}

	// hide current panel and show requested panel
	$(".visiblepanel").hide();
	$(".visiblepanel").removeClass("visiblepanel");
	$(".wfid_" + panelid).show();
	$(".wfid_" + panelid).addClass("visiblepanel");

	// if shown panel is anp-panel, then scroll to its previous position
	if (panelid == "anpcandidate_answerpaper") {
		ANP.show_on_screen_widgets();
		if(switched) {
			var aTag = $("a[id='"+ aname +"']");
			(document.querySelector(".page-nav-widget-main")).classList.add("hidden");
			// TODO fix this calculation.
			$('html,body').animate({scrollTop: aTag.offset().top - ANP.OFFSET_TOP}, 'fast');
		}
	} else {
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
					left: posX,
					top: posY,
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
	if (canvasdata != "false") {
		canvas.loadFromJSON(canvasdata);
	} else {
		ANP.setBackgroundImage(canvas, imgurl);
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
	var canvas = ANP.canvasobjs[canvasid];
	if (page && page.canvas_save)
		page.canvas_save(canvasid, JSON.stringify(canvas));
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

ANP.get_page_no = function(){
	var scrolled = $(document).scrollTop() + ANP.OFFSET_TOP;
	var pageHeight = ANP.BG_HEIGHT ;

	pageNo = 1 + Math.floor(scrolled/pageHeight);

	return pageNo;
}

///////////////////////////////////////////////////////////////////////////////

ANP.get_total_no_pages = function() {
  review_area = document.querySelector('.all_pages');
	return review_area.getElementsByTagName('a').length;
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
ANP.update_page_no_display = function() {
	$('#navbar_page_no').text("".concat(ANP.get_page_no(), "/", ANP.get_total_no_pages(), "  " ));
	//color active page button as blue
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
	let dropdown = document.querySelector('.navbar-collapse');
	if (dropdown) {
	    dropdown.style.zIndex = "1021";
	}
};


////////////////////////////////////////////////////////////////////////////////
//
// dynamic progress bar
//
////////////////////////////////////////////////////////////////////////////////
ANP.update_evaluation_progress_level = function() {
	let newprogress = 46;
	//
	//
	$('.progress-bar').attr('aria-valuenow', newprogress).css('width', newprogress+'%');
};


////////////////////////////////////////////////////////////////////////////////
//
// Hide widgets on comments panel
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


///////////////////////////////////////////////////////////////////////////////
//
// One time necessary init functions
//
///////////////////////////////////////////////////////////////////////////////
$(document).ready(function() {
	ANP.fixForStickyNavbar();
	ANP.raiseDropDownOverStickyNavbar();
	ANP.update_page_no_display();
	ANP.update_evaluation_progress_level();
	ANP.enable_navbar_fullscreen_button();

	//
	// remove unnecessary padding from review area
	//
	$("main").removeClass("px-3 py-4");
	$(".container-fluid").css('margin', '0');
	$(".container-fluid").css('padding', '0');

///////////////////////////////////////////////////////////////////////////////
//
// FLOATING TOOLBAR BUTTONS LISTENERS
//
///////////////////////////////////////////////////////////////////////////////
	var flipButton = document.getElementById("toolbar_flip");
	if (flipButton) {
		flipButton.addEventListener("click", function() {
			ANP.clicked_flip(ANP.get_active_canvasID());

		})
	}

	var rotateButton = document.getElementById("toolbar_rotate");
	if (rotateButton) {
		rotateButton.addEventListener("click", function() {
			ANP.clicked_rotate(ANP.get_active_canvasID());

		})
	}

	var eraseButton = document.getElementById("toolbar_eraseall");
	if (eraseButton) {
		eraseButton.addEventListener("click", function() {
			ANP.clicked_clear(ANP.get_active_canvasID());

		})
	}

	var undoButton = document.getElementById("toolbar_undo");
	if (undoButton) {
		undoButton.addEventListener("click", function() {
			ANP.clicked_undo(ANP.get_active_canvasID());

		})
	}

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
	var marks_box = document.getElementById("marks_box");
	var marks_box_mscheme = document.getElementById("marks_box_mscheme");

	marks_box_mscheme.classList.remove("hidden");
	marks_box_mscheme.classList.add("marks-box-container-large");
	$('.wfid_btn_submit_marks').removeClass('hidden');
}

function collapse_marks_box() {
	var marks_box = document.getElementById("marks_box");
	var marks_box_mscheme = document.getElementById("marks_box_mscheme");

	$('.wfid_btn_submit_marks').addClass('hidden');
	marks_box_mscheme.classList.add("hidden");
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

document.addEventListener("scroll", function(event) {
		ANP.update_page_no_display();
});


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
  if (event.ctrlKey && event.key === 'z') {
    ANP.clicked_undo(ANP.get_active_canvasID());
  }
});

///////////////////////////////////////////////////////////////////////////////
