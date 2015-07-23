-module(syntaxerl_xrl).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-behaviour(syntaxerl).
-export([check_syntax/2]).

-include("check_syntax_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

check_syntax(FileName, _Debug) ->
    case leex:file(FileName, [{report, true}, {return, true}]) of
        {ok, ScannerFile} ->
            file:delete(ScannerFile),
            {ok, []};
        {ok, ScannerFile, Warnings} ->
            file:delete(ScannerFile),
            {ok, format_warnings(Warnings)};
        {error, Errors, Warnings} ->
            case format_errors(Errors) of
                [] ->
                    {ok, format_warnings(Warnings)};
                Errors2 ->
                    {error, Errors2 ++ format_warnings(Warnings)}
            end
    end.

%% ===================================================================
%% Internal
%% ===================================================================

output_error(_) -> true.

output_warning(_) -> true.

fix_line_number(none) -> 1;
fix_line_number(Line) -> Line.

format_errors([]) ->
    [];
format_errors([{_FileName, Errors} | _]) ->
    FilteredErrors = lists:filter(fun output_error/1, Errors),
    lists:map(fun format_error/1, FilteredErrors).

format_error({Line, _Mod, _Term} = Error) ->
    Description = syntaxerl_utils:error_description(Error),
    FixedLine = fix_line_number(Line),
    {error, FixedLine, Description}.

format_warnings([]) ->
    [];
format_warnings([{_FileName, Warnings} | _]) ->
    FilteredWarnings = lists:filter(fun output_warning/1, Warnings),
    lists:map(fun format_warning/1, FilteredWarnings).

format_warning({Line, _Mod, _Term} = Error) ->
    Description = syntaxerl_utils:error_description(Error),
    FixedLine = fix_line_number(Line),
    {warning, FixedLine, Description}.
