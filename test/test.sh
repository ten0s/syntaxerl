#!/usr/bin/env bash

SCRIPT_DIR=$(dirname ${BASH_SOURCE[0]})
SYNTAXERL=${SCRIPT_DIR}/../syntaxerl

EXIT=0

function check() {
    local file="$1"
    local code="$3"
    local predicate="$4"
    local pattern="$5"

    echo -en "${file}\t"

    output=$(${SYNTAXERL} ${SCRIPT_DIR}/${file})
    ret=$?
    echo -n code: ${code}/${ret} " "

    if [[ ${ret} == ${code} && ${predicate} == "w/" && ${pattern} == "" ]]; then
        echo -e "\e[32mOK\e[0m"
    elif [[ ${ret} == ${code} ]]; then
        echo ${output} | grep "${pattern}" > /dev/null
        ret2=$?
        echo -n grep: ${ret2} " "
        if [[ ${ret2} == 0 && "${predicate}" == "w/" ]]; then
            echo -e "\e[32mOK\e[0m"
        elif [[ ${ret2} == 1 && "${predicate}" == "w/o" ]]; then
            echo -e "\e[32mOK\e[0m"
        else
            echo -e "\e[31mFAIL\e[0m"
            EXIT=1
        fi
    else
        echo -e "\e[31mFAIL\e[0m"
        EXIT=1
    fi
}

check hrl_ok.hrl      code 0 w/ ""
check hrl_warning.hrl code 0 w/ "hrl_warning.hrl:4: warning: function test/0 is unused"
check hrl_error.hrl   code 1 w/ "hrl_error.hrl:4: syntax error before: '}'"

check escript_ok      code 0 w/ ""
check escript_warning code 0 w/ "escript_warning:5: warning: missing specification for function main/1"
check escript_error   code 1 w/ "escript_error:7: function main/1 already defined"

check escript_ok.erl      code 0 w/ ""
check escript_warning.erl code 0 w/ "escript_warning.erl:5: warning: missing specification for function main/1"
check escript_error.erl   code 1 w/ "escript_error.erl:7: function main/1 already defined"

check escript_ok.es      code 0 w/ ""
check escript_warning.es code 0 w/ "escript_warning.es:5: warning: missing specification for function main/1"
check escript_error.es   code 1 w/ "escript_error.es:7: function main/1 already defined"

check escript_ok.escript      code 0 w/ ""
check escript_warning.escript code 0 w/ "escript_warning.escript:5: warning: missing specification for function main/1"
check escript_error.escript   code 1 w/ "escript_error.escript:7: function main/1 already defined"

check escript_with_module_ok.escript code 0 w/ ""

check escript_no_main.escript code 0 w/ ""

check script_ok.script code 0 w/ ""
check script_error.script code 1 w/ "{unbound_var,'ExtraDeps'}"

check terms_ok.config code 0 w/ ""
check terms_error.config code 1 w/ "terms_error.config:10: syntax error before: '{'"

check projects/default/src/src.erl code 0 w/ ""
check projects/rebar-apps/apps/app1/src/src.erl code 0 w/ ""
check projects/rebar3-apps/apps/app1/src/src.erl code 0 w/ ""
check projects/rebar3-apps/apps/app1/src/subdir/subsrc.erl code 0 w/ ""
check projects/rebar3-apps/_build/default/lib/lib2/src/src.erl code 0 w/ ""
check projects/rebar3-apps/apps/app1/src/types.erl code 0 w/ ""

exit ${EXIT}
