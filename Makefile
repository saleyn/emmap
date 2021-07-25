REBAR=rebar3

all: deps compile

compile:
	@$(REBAR) compile eunit

clean:
	@$(REBAR) clean

deps:
	@$(REBAR) get-deps

test eunit:
	@$(REBAR) eunit
