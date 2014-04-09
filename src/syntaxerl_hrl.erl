-module(syntaxerl_hrl).
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
            %% precede with the module name, so now this is a real erlang module.
            NewContent = <<
                <<"-module(fixed_inc).\n">>/binary,
                Content/binary
            >>,
            %% append an erlang extention, so the `compile:file' won't complain.
            NewFileName = FileName ++ ".erl",
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
                            {error, format_errors(Errors) ++ format_warnings(Warnings)}
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

%% skip errors that might occur in pure header files.
output_error({_, _, {spec_fun_undefined, _}}) -> false;
output_error(_) -> true.

%% skip warnings that might occur in pure header files.
output_warning({_, _, {unused_record, _}}) -> false;
output_warning({_, _, {unused_type, _}}) -> false;
output_warning(_) -> true.

%% appropriately fix line numbers due to the `-module' definition.
fix_line_number(none) -> 1;
fix_line_number(Line) -> Line - 1.

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
