-module(syntaxerl_yrl).
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
    case yecc:file(FileName, [{report, true}, {return, true}]) of
        {ok, ScannerFile} ->
            file:delete(ScannerFile),
            {ok, []};
        {ok, ScannerFile, Warnings} ->
            file:delete(ScannerFile),
            {ok, syntaxerl_format:format_warnings(?MODULE, Warnings)};
        {error, Errors, Warnings} ->
            case syntaxerl_format:format_errors(?MODULE, Errors) of
                [] ->
                    {ok, syntaxerl_format:format_warnings(?MODULE, Warnings)};
                Errors2 ->
                    {error, Errors2 ++ syntaxerl_format:format_warnings(?MODULE, Warnings)}
            end
    end.

output_error(_) -> true.

output_warning(_) -> true.
