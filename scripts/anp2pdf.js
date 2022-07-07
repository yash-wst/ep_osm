///////////////////////////////////////////////////////////////////////////////

var http = require('http');
var fs = require('fs');
var fabric = require('/usr/local/lib/node_modules/fabric').fabric;

var arguments = process.argv.slice(2);
var anptype = arguments[2];
var un = arguments[3];
var pw = arguments[4];
var dbhost = arguments[5];
var dbport = arguments[6];
var dbprefix = arguments[7];
var object_prefix_s3 = arguments[8];
var object_prefix_local = arguments[9];
var auth = un + ':' + pw;

if (dbprefix == "undefined")
	var path = '/anp_' + arguments[0] + '/' + arguments[1];
else
	var path = '/' + dbprefix + 'anp_' + arguments[0] + '/' + arguments[1];

var oex = {};

///////////////////////////////////////////////////////////////////////////////

oex.createImages = function(list) {
	for (i in list) {
		var imageObj = list[i];
		for (k in imageObj) {
			oex.createPNG(k, imageObj[k]);
		}
	}
};

oex.createPNG = function(imageName, imageJson0) {

	//
	// init
	//
	var Width = 1300;
	var Height = 900;
	var canvas = new fabric.StaticCanvas(null, {width:Width, height:Height});
	imageJson = JSON.parse(imageJson0);
	imageJson.backgroundImage.width = null;
	imageJson.backgroundImage.height = null;


	canvas.loadFromJSON(imageJson, function() {

		canvas.backgroundImage.scaleToWidth(Width);
		canvas.backgroundImage.scaleToHeight(Height);
		canvas.renderAll();

		var out = fs.createWriteStream(imageName);
		var stream = canvas.createPNGStream();
		stream.on('data', function(chunk) {
			out.write(chunk);
		});

	});
};


///////////////////////////////////////////////////////////////////////////////

var options = {
	host: dbhost,
	port: parseInt(dbport),
	path: path,
	auth: auth,
	headers: {
		'Referer': 'https://osm.uniapps.in/'
	}
};

http.get(options, function(resp) {
	var body = '';

	resp.on('data', function(chunk) {
		body += chunk;
	});

	resp.on('end', function() {

		var body1 = body.split(object_prefix_s3).join(object_prefix_local);
		var json = JSON.parse(body1);
		var canvasJson = json[anptype];

		oex.createImages(canvasJson);

	});

});

///////////////////////////////////////////////////////////////////////////////
