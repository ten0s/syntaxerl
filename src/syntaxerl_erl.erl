-module(syntaxerl_erl).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-behaviour(syntaxerl).
-export([check_syntax/2]).

-include("check_syntax_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

check_syntax(FileName, Debug) ->
    {InclDirs, DepsDirs, ErlcOpts} = syntaxerl_utils:incls_deps_opts(FileName),
    syntaxerl_logger:debug(Debug, "Include dirs: ~p", [InclDirs]),
    syntaxerl_logger:debug(Debug, "Deps dirs: ~p", [DepsDirs]),
    syntaxerl_logger:debug(Debug, "Erlc opts: ~p", [ErlcOpts]),

    code:add_paths(DepsDirs),

    Result = compile:file(FileName, ErlcOpts ++ InclDirs),
    syntaxerl_logger:debug(Debug, "Compile result: ~p", [Result]),

    case Result of
        {ok, _ModuleName} ->
            {ok, []};
        {ok, _ModuleName, Warnings} ->
            {ok, format_warnings(Warnings)};
        {error, Errors, Warnings} ->
            {error, format_errors(Errors) ++ format_warnings(Warnings)}
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
