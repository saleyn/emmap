REBAR=./rebar
ERL=erl

all: deps compile

./rebar:
	$(ERL) \
		-noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"http://github.com/downloads/basho/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

compile: $(REBAR)
	@$(REBAR) compile eunit

clean: $(REBAR)
	@$(REBAR) clean

deps: $(REBAR)
	@$(REBAR) check-deps || (export GPROC_DIST=true; $(REBAR) get-deps)