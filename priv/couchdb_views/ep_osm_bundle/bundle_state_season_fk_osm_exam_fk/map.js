function (doc) {
	if (doc.bundle_size) {

		var bundle_state = "inward_completed";
		if(doc.scanningstate == "completed")
			bundle_state = "scanning_completed";
		if(doc.uploadstate == "completed")
			bundle_state == "upload_completed"

		emit([bundle_state, doc.season_fk, doc.osm_exam_fk], parseInt(doc.bundle_size, 10));
	}
}
