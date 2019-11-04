-module(syntaxerl_format).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-export([
    format_errors/2,
    format_warnings/2
]).

-include("issues_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec format_errors(module(), error_list()) -> [error()].
format_errors(Handler, List) ->
    lists:flatmap(fun ({FileName, Errors}) ->
        [format_error(FileName, E) || E <- Errors, Handler:output_error(E)]
    end, List).

-spec format_warnings(module(), warning_list()) -> [warning()].
format_warnings(Handler, List) ->
    lists:flatmap(fun ({FileName, Warnings}) ->
        [format_warning(FileName, W) || W <- Warnings, Handler:output_warning(W)]
    end, List).

%% ===================================================================
%% Internal
%% ===================================================================

format_error(FileName, {Line, _Module, _Term} = Error) ->
    Description = error_description(Error),
    FixedLine = fix_line_number(Line),
    {error, FileName, FixedLine, Description}.

format_warning(FileName, {Line, _Module, _Term} = Error) ->
    Description = error_description(Error),
    FixedLine = fix_line_number(Line),
    {warning, FileName, FixedLine, Description}.

-spec error_description({integer(), module(), term()}) -> string().
error_description({_Line, Module, Error}) ->
    Module:format_error(Error).

fix_line_number(none) -> 1;
fix_line_number(Line) -> Line.
