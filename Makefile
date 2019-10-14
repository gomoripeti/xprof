JS_PRIV=apps/xprof_gui/priv
BIN_DIR:=node_modules/.bin
VERSION:=$(shell grep vsn apps/xprof/src/xprof.app.src | cut -d '"' -f 2)
DOC_EBIN_DIR=./doc/ebin

# this will update cowboy version based on rebar.config overwriting the lock file
ifdef COWBOY_VERSION
	MAYBE_UPDATE_COWBOY = ./rebar3 upgrade cowboy
endif
ifdef XPROF_ERL_HIST
	MAYBE_UNLOCK_HIST = ./rebar3 unlock hdr_histogram
endif

compile:
	$(MAYBE_UPDATE_COWBOY)
	$(MAYBE_UNLOCK_HIST)
	./rebar3 compile

dev: dev_front_end dev_back_end

dev_back_end:
	$(MAYBE_UPDATE_COWBOY)
	$(MAYBE_UNLOCK_HIST)
	./rebar3 as dev compile, shell

dev_front_end:
	cd $(JS_PRIV); npm run start:with-cowboy &

npm:
	cd $(JS_PRIV); npm install

bootstrap_front_end: npm

test_front_end:
	cd $(JS_PRIV); npm run test:single-run

build_prod_front_end:
	cd $(JS_PRIV); npm run build

test: compile
	$(MAYBE_UPDATE_COWBOY)
	$(MAYBE_UNLOCK_HIST)
	./rebar3 do eunit -c, ct -c, cover

doc:
	./rebar3 edoc

~/.mix/escripts/ex_doc:
	# mix escript.install hex ex_doc --force
	mix escript.install github elixir-lang/ex_doc branch wm-erlang

./doc/src/readme.md: README.md
	sed -e 's|(doc/src/querysyntax.md)|(querysyntax.html)|' \
	    -e 's|doc/assets/|assets/|' \
	    -e '1 s|\[!\[.*||' README.md > ./doc/src/readme.md

# This step requires a relatively new rebar3 (somewhere between 3.5.2 and 3.13.0)
# Older versions set source in compile module_info to `_build/docs/lib/xprof_core/src/xprof_core.erl`
# instead of `apps/xprof_core/src/xprof_core.erl`
# which results in wrong links in ex_doc to github source code
$(DOC_EBIN_DIR)/xprof_core.beam: ./apps/xprof_core/src/xprof_core.erl _build/docs/lib/xprof_core/ebin/xprof_core.beam
	./rebar3 as docs compile
	mkdir -p $(DOC_EBIN_DIR)
	cp _build/docs/lib/xprof_core/ebin/xprof_core.beam $(DOC_EBIN_DIR)/xprof_core.beam

gen_ex_doc: ~/.mix/escripts/ex_doc ./doc/docs.exs ./doc/src/readme.md $(DOC_EBIN_DIR)/xprof_core.beam
	~/.mix/escripts/ex_doc XProf $(VERSION) $(DOC_EBIN_DIR) -c ./doc/docs.exs

dialyzer:
	$(MAYBE_UPDATE_COWBOY)
	$(MAYBE_UNLOCK_HIST)
	./rebar3 dialyzer

publish:
	./rebar3 as publish hex publish --deps_from_config

override_edoc: gen_ex_doc
	rm -r apps/xprof/doc/*
	cp -R doc/html/* apps/xprof/doc/

publish_docs:
	./rebar3 as publish hex docs

.PHONY: compile dev dev_back_end dev_front_end npm bootstrap_front_end test_front_end build_prod_front_end test doc gen_ex_doc dialyzer publish publish_docs
