all:
	rebar co
clean:
	rebar clean
app:
	echo ${EPAPP}

	cp -r ../ep_template ../${EPAPP}

	rm -f ../${EPAPP}/ebin/*

	mv ../${EPAPP}/etc/ep_template.config ../${EPAPP}/etc/${EPAPP}.config
	sed -i.bak "s/ep_template/${EPAPP}/g" ../${EPAPP}/etc/${EPAPP}.config
	rm ../${EPAPP}/etc/${EPAPP}.config.bak


	mv ../${EPAPP}/src/ep_template_app.erl ../${EPAPP}/src/${EPAPP}_app.erl
	mv ../${EPAPP}/src/ep_template_sup.erl ../${EPAPP}/src/${EPAPP}_sup.erl
	mv ../${EPAPP}/src/ep_template_setup.erl ../${EPAPP}/src/${EPAPP}_setup.erl
	mv ../${EPAPP}/src/ep_template_menu.erl ../${EPAPP}/src/${EPAPP}_menu.erl
	mv ../${EPAPP}/src/ep_template.app.src ../${EPAPP}/src/${EPAPP}.app.src
	sed -i.bak "s/ep_template/${EPAPP}/g" ../${EPAPP}/src/${EPAPP}_app.erl
	sed -i.bak "s/ep_template/${EPAPP}/g" ../${EPAPP}/src/${EPAPP}_sup.erl
	sed -i.bak "s/ep_template/${EPAPP}/g" ../${EPAPP}/src/${EPAPP}_setup.erl
	sed -i.bak "s/ep_template/${EPAPP}/g" ../${EPAPP}/src/${EPAPP}_menu.erl
	sed -i.bak "s/ep_template/${EPAPP}/g" ../${EPAPP}/src/${EPAPP}.app.src
	rm ../${EPAPP}/src/${EPAPP}_app.erl.bak
	rm ../${EPAPP}/src/${EPAPP}_sup.erl.bak
	rm ../${EPAPP}/src/${EPAPP}_setup.erl.bak
	rm ../${EPAPP}/src/${EPAPP}_menu.erl.bak
	rm ../${EPAPP}/src/${EPAPP}.app.src.bak

	rm ../${EPAPP}/scripts/module.escript
	rm -rf ../${EPAPP}/.git