.PHONY: doc

REBAR=./rebar

all: compile escriptize

escriptize: compile
	@$(REBAR) escriptize

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

tags:
	@find . -name "*.[e,h]rl" -print | etags -
