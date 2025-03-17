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

doc docs:
	@$(REBAR) ex_doc

test eunit:
	@TERM=dumb $(REBAR) eunit

nif:
	make -C c_src

publish: docs clean
	$(REBAR) hex $(if $(replace),publish --replace,cut)

.PHONY: test doc
