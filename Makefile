.PHONY: doc test

REBAR=./rebar

all: escriptize

escriptize: compile xref
	@$(REBAR) escriptize

compile:
	@$(REBAR) compile

xref: compile
	@$(REBAR) xref skip_deps=true

clean:
	@$(REBAR) clean

test:
	@test/test.sh

tags:
	@find . -name "*.[e,h]rl" -print | etags -
