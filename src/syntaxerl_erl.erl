-module(syntaxerl_erl).
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

check_syntax(FileName, BaseFileName, Debug) ->
    {InclDirs, DepsDirs, ErlcOpts} = syntaxerl_utils:incls_deps_opts(BaseFileName),
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
