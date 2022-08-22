REBAR=rebar3

all: deps compile

compile:
	@TERM=dumb $(REBAR) compile

clean:
	@$(REBAR) clean
	@make -C c_src clean >/dev/null 2>&1

deps:
	@$(REBAR) get-deps

test eunit:
	@TERM=dumb $(REBAR) eunit

nif:
	make -C c_src

docs:
	@$(REBAR) edoc

github-docs gh-pages: VSN=$(shell git describe --always --tags --abbrev=1 | sed 's/^v//')
github-docs gh-pages:
	make docs
	make clean
	@if git branch | grep -q gh-pages ; then \
		git checkout gh-pages; \
	else \
		git checkout -b gh-pages; \
	fi
	rm -f rebar.lock
	mv doc/*.* .
	rm -fr src c_src include Makefile *.*dump priv rebar.* README* _build ebin doc
	@FILES=`git st -uall --porcelain | sed -n '/^?? [A-Za-z0-9]/{s/?? //p}'`; \
	for f in $$FILES ; do \
		echo "Adding $$f"; git add $$f; \
	done
	# Commit & push changes to origin, switch back to master, and restore 'doc' directory
	@sh -c "ret=0; set +e; \
		if   git commit -a --amend -m 'Documentation updated'; \
		then git push origin +gh-pages; echo 'Pushed gh-pages to origin'; \
		else ret=1; git reset --hard; \
		fi; \
		set -e; \
    git checkout master && echo 'Switched to master' && mkdir doc && git --work-tree=doc checkout gh-pages -- .; \
    exit $$ret"
