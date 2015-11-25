#!/bin/bash

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

check escript_ok.erl      code 0 w/ ""
check escript_warning.erl code 0 w/ "escript_warning.erl:5: warning: missing specification for function main/1"
check escript_error.erl   code 1 w/ "escript_error.erl:7: function main/1 already defined"

check escript_ok.escript      code 0 w/ ""
check escript_warning.escript code 0 w/ "escript_warning.escript:5: warning: missing specification for function main/1"
check escript_error.escript   code 1 w/ "escript_error.escript:7: function main/1 already defined"

exit ${EXIT}
