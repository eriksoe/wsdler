compile:
	ERL_LIBS=$ERL_LIBS:deps/ erl -make </dev/null

setup:
	./rebar get-deps </dev/null && ./rebar compile </dev/null

test:
	./rebar eunit skip_deps=true </dev/null

dialyze: compile
	dialyzer -q -n ebin

doc:
	for i in doc/*.asciidoc; do ${MAKE} $${i%.asciidoc}.html ; done

doc/%.html: doc/%.asciidoc
	asciidoc $<

.PHONY: compile setup test dialyze doc
