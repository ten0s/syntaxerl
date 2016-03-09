-module(syntaxerl_script).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-behaviour(syntaxerl).

-export([
    check_syntax/2,
    output_error/1,
    output_warning/1
]).

-include("check_syntax_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

check_syntax(FileName, _Debug) ->
    case syntaxerl_utils:consult_file(FileName) of
        {ok, _} ->
            {ok, []};
        {error, Error} ->
            {error, [{error, file:format_error(Error)}]}
    end.

output_error(_) -> true.

output_warning(_) -> true.
