.PHONY: doc

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

tags:
	@find . -name "*.[e,h]rl" -print | etags -
