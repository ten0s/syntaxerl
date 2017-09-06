-module(syntaxerl_script).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-behaviour(syntaxerl).

-export([
    check_syntax/3,
    output_error/1,
    output_warning/1,
    format_error/2
]).

-include("check_syntax_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

check_syntax(FileName, _BaseFileName, _Debug) ->
    case syntaxerl_utils:consult_file(FileName) of
        {ok, _} ->
            {ok, []};
        {error, Error} ->
            %% unfortunately the `file:consult' returns only the first error.
            format_error(FileName, Error)
    end.

output_error(_) -> true.

output_warning(_) -> true.

%% ===================================================================
%% Internal
%% ===================================================================

-spec format_error(file:filename(), Error) -> {ok, term()} | {error, error()}
    when Error :: file:posix() | badarg | terminated | system_limit |
                  {Line :: integer(), Mod :: module(), Term :: term()}.
format_error(FileName, {Line, Mod, Term}) ->
    ErrorStrPrefix = iolist_to_binary(io_lib:format("~p: ", [Line])),
    ErrorStr = iolist_to_binary(file:format_error({Line, Mod, Term})),
    [<<>>, ErrorStr0] = binary:split(ErrorStr, ErrorStrPrefix),
    {error, [{error, FileName, Line, ErrorStr0}]};
format_error(FileName, Error) ->
    {error, [{error, FileName, 1, file:format_error(Error)}]}.
