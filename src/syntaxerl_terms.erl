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
    case file:consult(FileName) of
        {ok, _} ->
            {ok, []};
        {error, Error} ->
            %% unfortunately the `file:consult' returns only the first error.
            syntaxerl_script:format_error(FileName, Error)
    end.

output_error(_) -> true.

output_warning(_) -> true.
