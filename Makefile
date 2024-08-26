.PHONY: doc test

REBAR3=rebar3

all: escriptize

escriptize: compile xref
	$(REBAR3) escriptize
	ln -fs _build/default/bin/syntaxerl syntaxerl

compile:
	$(REBAR3) compile

xref: compile
	$(REBAR3) xref

dialyze:
	$(REBAR3) dialyzer

clean:
	$(REBAR3) clean
	rm -f syntaxerl

test:
	test/test.sh

tags:
	find . -name "*.[e,h]rl" -print | etags -
