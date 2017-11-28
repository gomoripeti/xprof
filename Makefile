JS_PRIV=apps/xprof_gui/priv
BIN_DIR:=node_modules/.bin
CACHEGRIND?=qcachegrind

# this will update cowboy version based on rebar.config overwriting the lock file
ifdef COWBOY_VERSION
	MAYBE_UPDATE_COWBOY = ./rebar3 upgrade cowboy
endif

compile:
	$(MAYBE_UPDATE_COWBOY)
	./rebar3 compile

dev: dev_front_end dev_back_end

dev_back_end:
	$(MAYBE_UPDATE_COWBOY)
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
	./rebar3 do eunit -c, ct -c, cover

doc:
	./rebar3 edoc

dialyzer:
	$(MAYBE_UPDATE_COWBOY)
	./rebar3 dialyzer

publish:
	./rebar3 as publish hex publish --deps_from_config

profile:
	@echo "Profiling..."
	@./rebar3 as profile compile
	@rm -f fprofx.*
	@erl +K true \
	     -noshell \
	     -pa _build/profile/lib/*/ebin \
	     -pa _build/profile/lib/*/test \
		 -eval 'xprof_profile:fprofx()' \
		 -eval 'init:stop()'
	@_build/profile/lib/fprofx/erlgrindx -p fprofx.analysis
	@$(CACHEGRIND) fprofx.cgrind

.PHONY: compile dev dev_back_end dev_front_end npm bootstrap_front_end test_front_end build_prod_front_end test doc dialyzer publish profile
