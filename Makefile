compile:
	ERL_LIBS=$ERL_LIBS:deps/ erl -make

setup:
	./rebar get-deps && ./rebar compile

test:
	./rebar eunit skip_deps=true

dialyze: compile
	dialyzer -q -n ebin

doc:
	for i in doc/*.asciidoc; do ${MAKE} $${i%.asciidoc}.html ; done

doc/%.html: doc/%.asciidoc
	asciidoc $<

.PHONY: compile setup test dialyze doc
