.PHONY: doc test

REBAR=./rebar

NAME=syntaxerl
OTP_PLT=~/.r18.3.plt
PRJ_PLT=.$(NAME).plt

all: escriptize

escriptize: compile xref
	@$(REBAR) escriptize

compile:
	@$(REBAR) compile

xref: compile
	@$(REBAR) xref skip_deps=true

dialyze: $(OTP_PLT) compile xref $(PRJ_PLT)
	@dialyzer --plt $(PRJ_PLT) -r ./ebin/ -Wno_improper_lists

$(OTP_PLT):
	@dialyzer --build_plt --output_plt $(OTP_PLT) --apps kernel stdlib erts   \
		crypto public_key mnesia sasl common_test eunit ssh ssl asn1 compiler \
		parsetools syntax_tools tools inets snmp xmerl

$(PRJ_PLT):
	@dialyzer --add_to_plt --plt $(OTP_PLT) --output_plt $(PRJ_PLT) \
		-r ./ebin/ || true

clean:
	@$(REBAR) clean

test:
	@test/test.sh

tags:
	@find . -name "*.[e,h]rl" -print | etags -
