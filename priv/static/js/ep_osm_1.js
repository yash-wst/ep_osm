///////////////////////////////////////////////////////////////////////////////

var ANP = {};

ANP.BG_WIDTH = 1300;
ANP.BG_HEIGHT = 900;

ANP.canvasobjs = {};
ANP.cursorText = undefined;

 ANP.OFFSET_TOP = 70;

 ANP.showing_marking_scheme = true;

///////////////////////////////////////////////////////////////////////////////

ANP.scrollposap = 0;
ANP.showpanel = function (panelid, switched, aname) {

	// record ap-panel scroll position
	if ($(".visiblepanel").hasClass("wfid_anpcandidate_answerpaper")) {
		ANP.scrollposap = $(document).scrollTop();
	}

	// check if we are navigating from page-panel
	var fromPages = $(".visiblepanel").hasClass("wfid_anpcandidate_pages");

	// hide current panel and show requested panel
	$(".visiblepanel").hide();
	$(".visiblepanel").removeClass("visiblepanel");
	$(".wfid_" + panelid).show();
	$(".wfid_" + panelid).addClass("visiblepanel");

	// if shown panel is ap-panel, then scroll to its previous position
	if (panelid == "anpcandidate_answerpaper") {

		// if coming from pages-panel, check of page has been switched.
		if (fromPages) {
			if(!switched) {
				$(document).scrollTop(ANP.scrollposap - ANP.OFFSET_TOP);
			}
			else {
				var aTag = $("a[id='"+ aname +"']");
				$('html,body').animate({scrollTop: aTag.offset().top - ANP.OFFSET_TOP}, 'fast');
			}
		} else {
			$(document).scrollTop(ANP.scrollposap - ANP.OFFSET_TOP);
		}
	} else {
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

$(document).mousemove(function(e) {
	ANP.set_cursor_tooltip(e);
});

///////////////////////////////////////////////////////////////////////////////

WstTimer = {};
WstTimer.secondselapsed = 0;

WstTimer.start = function (seconds) {
	WstTimer.TimerId = window.setInterval(WstTimer.onTimeOut, 1000);
	WstTimer.secondselapsed = seconds;
};

WstTimer.onTimeOut = function () {
	var SecondsToAdd = 1;
	WstTimer.secondselapsed = WstTimer.secondselapsed + SecondsToAdd;

	if ((WstTimer.secondselapsed % 60 ==0)) {
		page.timer_event(WstTimer.secondselapsed);
	};

	WstTimer.updateElement();
};

WstTimer.updateElement = function () {
	var mins = Math.floor(WstTimer.secondselapsed / 60);
	var secs = WstTimer.secondselapsed % 60;

	var html = "<span id='time_spend'>" + mins + "m " + secs + "s" + "</span>";

	$("#time_spend").html(html);
};

WstTimer.unload = function () {
	window.clearInterval(WstTimer.TimerId);
	page.unload(WstTimer.secondselapsed);
};


///////////////////////////////////////////////////////////////////////////////


$(document).mousemove(function(event) {
	if(ANP.showing_marking_scheme == false && event.pageX < 20) {
		ANP.showing_marking_scheme = true;
		$("#sidebar-wrapper").css("left", "20%");
	} else if(ANP.showing_marking_scheme == true && event.pageX > 250) {
		ANP.showing_marking_scheme = false;
		$("#sidebar-wrapper").css("left", "0%");
	}
});


///////////////////////////////////////////////////////////////////////////////

$(document).ready(function() {

	var viewFullScreen = document.getElementById("view-fullscreen");

	if (viewFullScreen) {
	  viewFullScreen.addEventListener("click", function() {

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

});

///////////////////////////////////////////////////////////////////////////////
