-module(syntaxerl_escript).
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
    case file:read_file(FileName) of
        {ok, Content} ->
            Preface = <<"-module(fixed_escript).\n-export([main/1]).\n">>,
            NewContent =
                case binary:split(Content, <<"\n">>) of
                    [<<"#!", _/binary>>, Content0] ->
                        <<Preface/binary, "\n", Content0/binary>>;
                    _Other -> <<Preface/binary, Content/binary>>
                end,
            NewFileName = filename:rootname(FileName, ".erl") ++ "_fixed.erl",
            case file:write_file(NewFileName, NewContent) of
                ok ->
                    {InclDirs, DepsDirs, ErlcOpts} = syntaxerl_utils:incls_deps_opts(BaseFileName),
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
                            io:format("Warnings: ~p~n", [Warnings]),
                            {ok, syntaxerl_format:format_warnings(
                                    ?MODULE, fix_line_numbers(Warnings))};
                        {error, Errors, Warnings} ->
                            case syntaxerl_format:format_errors(
                                    ?MODULE, fix_line_numbers(Errors)) of
                                [] ->
                                    {ok, syntaxerl_format:format_warnings(
                                            ?MODULE, fix_line_numbers(Warnings))};
                                Errors2 ->
                                    {error, Errors2 ++ syntaxerl_format:format_warnings(
                                        ?MODULE, fix_line_numbers(Warnings))}
                            end
                    end;
                {error, Reason} ->
                    {error, [{error, file:format_error(Reason)}]}
            end;
        {error, Reason} ->
            {error, [{error, file:format_error(Reason)}]}
    end.

output_error(_) -> true.

output_warning(_) -> true.

%% ===================================================================
%% Internal
%% ===================================================================

fix_line_numbers(ErrorList) ->
    ErrorList0 = skip_expected_errors(ErrorList),
    [{F, [{fix_line_number(L), M, E} || {L, M, E} <- Es]}
     || {F, Es} <- ErrorList0].

%% appropriately fix line numbers due to the `-module' definition.
fix_line_number(none) -> 1;
fix_line_number(Line) when Line < 3 -> 1;
fix_line_number(Line) -> Line - 2.

skip_expected_errors(ErrorList) ->
    [{F, lists:keydelete(redefine_module, 3,
         lists:keydelete({duplicated_export, {main, 1}}, 3, Es))}
     || {F, Es} <- ErrorList].
