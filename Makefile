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

clean:
	rm ebin/*.beam;
	rm test/*.beam;
	rm -r .eunit
	rm ebin/*~
	rm test/*~

.IGNORE: clean

.PHONY: compile setup test dialyze doc
