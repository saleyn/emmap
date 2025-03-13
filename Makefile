REBAR=rebar3

all: deps compile

compile:
	@TERM=dumb $(REBAR) compile

clean:
	@$(REBAR) clean
	@make -C c_src clean >/dev/null 2>&1

deps:
	@$(REBAR) get-deps

check:
	@$(REBAR) dialyzer

test eunit:
	@TERM=dumb $(REBAR) eunit

nif:
	make -C c_src

-include build-aux/docs-addon.mk

build-aux/docs-addon.mk:
	@echo "Fetching build-aux/docs-addon.mk" && \
		mkdir -p build-aux && \
		curl -s -o build-aux/docs-addon.mk https://raw.githubusercontent.com/saleyn/util/master/build-aux/docs-addon.mk

publish: docs clean
	$(REBAR) hex $(if $(replace),publish --replace,cut)

.PHONY: test
