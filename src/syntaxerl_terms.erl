-module(syntaxerl_terms).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-behaviour(syntaxerl).

-export([
    check_syntax/3,
    output_error/1,
    output_warning/1
]).

-include("check_syntax_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

check_syntax(FileName, _BaseFileName, _Debug) ->
    case file:eval(FileName) of
        ok ->
            {ok, []};
        {error, Error} ->
            %% unfortunately the `file:eval' returns only the first error.
            {error, [{error, file:format_error(Error)}]}
    end.

output_error(_) -> true.

output_warning(_) -> true.
