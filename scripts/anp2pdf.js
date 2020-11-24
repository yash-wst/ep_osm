///////////////////////////////////////////////////////////////////////////////

var http = require('http');
var fs = require('fs');
var fabric = require('/usr/local/lib/node_modules/fabric').fabric;

var arguments = process.argv.slice(2);
var path = '/anp_' + arguments[0] + '/' + arguments[1];
var anptype = arguments[2];
var un = arguments[3];
var pw = arguments[4];
var dbhost = arguments[5];
var dbport = arguments[6];
var auth = un + ':' + pw;

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

oex.createPNG = function(imageName, imageJson) {

	// console.log(imageName);
	// console.log(imageJson);

	var canvas = new fabric.StaticCanvas(null, {width:900,height:1800});

	canvas.loadFromJSON(imageJson, function() {
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
	auth: auth
};

http.get(options, function(resp) {
	var body = '';

	resp.on('data', function(chunk) {
		body += chunk;
	});

	resp.on('end', function() {
		var json = JSON.parse(body)
		var canvasJson = json[anptype];

		oex.createImages(canvasJson);

	});

});

///////////////////////////////////////////////////////////////////////////////