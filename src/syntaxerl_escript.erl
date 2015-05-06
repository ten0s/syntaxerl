-module(syntaxerl_escript).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-behaviour(syntaxerl).
-export([check_syntax/2]).

-include("check_syntax_spec.hrl").

%% ===================================================================
%% API
%% ===================================================================

check_syntax(FileName, Debug) ->
    case file:read_file(FileName) of
        {ok, Content} ->
            %% replace shebang line with module definition.
            NewContent = re:replace(Content, <<"#.*">>, <<"-module(fixed_escript).">>),
            %% make a new file name.
            NewFileName = filename:rootname(FileName, ".erl") ++ "_fixed.erl",
            case file:write_file(NewFileName, NewContent) of
                ok ->
                    {InclDirs, DepsDirs, ErlcOpts} = syntaxerl_utils:incls_deps_opts(FileName),
                    syntaxerl_logger:debug(Debug, "Include dirs: ~p", [InclDirs]),
                    syntaxerl_logger:debug(Debug, "Deps dirs: ~p", [DepsDirs]),
                    syntaxerl_logger:debug(Debug, "Erlc opts: ~p", [ErlcOpts]),

                    code:add_paths(DepsDirs),

                    Result = compile:file(NewFileName, ErlcOpts ++ InclDirs),
                    syntaxerl_logger:debug(Debug, "Compile result: ~p", [Result]),

                    file:delete(NewFileName),

                    case Result of
                        {ok, _ModuleName} ->
                            {ok, []};
                        {ok, _ModuleName, Warnings} ->
                            {ok, format_warnings(Warnings)};
                        {error, Errors, Warnings} ->
                            case format_errors(Errors) of
                                [] ->
                                    {ok, format_warnings(Warnings)};
                                Errors2 ->
                                    {error, Errors2 ++ format_warnings(Warnings)}
                            end
                    end;
                {error, Reason} ->
                    {error, [{error, file:format_error(Reason)}]}
            end;
        {error, Reason} ->
            {error, [{error, file:format_error(Reason)}]}
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
