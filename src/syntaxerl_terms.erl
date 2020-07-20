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
    case file:open(FileName, [read]) of
        {ok, Fd} ->
            R = check_syntax(Fd),
            _ = file:close(Fd),
            R;
        {error, Error} ->
            {error, [format_error(Error)]}
    end.

output_error(_) -> true.

output_warning(_) -> true.

%% ===================================================================
%% Internal
%% ===================================================================

check_syntax(Fd) ->
    _ = epp:set_encoding(Fd),
    case find_errors(Fd, 1, []) of
        [] ->
            {ok, []};
        Errors ->
            {error, [format_error(E) || E <- Errors]}
    end.

find_errors(Fd, Line, Errors) ->
    case io:read(Fd, '', Line) of
        {ok, _, EndLine} ->
            find_errors(Fd, EndLine, Errors);
        {error, Error, EndLine} ->
            find_errors(Fd, EndLine, [Error | Errors]);
        {eof, _} ->
            lists:reverse(Errors)
    end.

format_error(Error) ->
    {error, file:format_error(Error)}.
