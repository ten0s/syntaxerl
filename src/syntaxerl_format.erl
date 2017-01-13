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

-spec format_errors(module(), error_list()) ->
    [{error, integer(), string()}].
format_errors(_Handler, []) ->
    [];
format_errors(Handler, [{_FileName, Errors} | _]) ->
    [format_error(E) || E <- Errors, Handler:output_error(E)].

-spec format_warnings(module(), warning_list()) ->
    [{warning, integer(), string()}].
format_warnings(_Handler, []) ->
    [];
format_warnings(Handler, [{_FileName, Warnings} | _]) ->
    [format_warning(W) || W <- Warnings, Handler:output_warning(W)].

%% ===================================================================
%% Internal
%% ===================================================================

format_error({Line, _Module, _Term} = Error) ->
    Description = error_description(Error),
    FixedLine = fix_line_number(Line),
    {error, FixedLine, Description}.

format_warning({Line, _Module, _Term} = Error) ->
    Description = error_description(Error),
    FixedLine = fix_line_number(Line),
    {warning, FixedLine, Description}.

-spec error_description({integer(), module(), term()}) -> string().
error_description({_Line, Module, Error}) ->
    Module:format_error(Error).

fix_line_number(none) -> 1;
fix_line_number(Line) -> Line.
