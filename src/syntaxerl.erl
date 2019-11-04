-module(syntaxerl).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").
-export([main/1]).

-ignore_xref([
    {behaviour_info, 1},
    {main, 1}
]).

-include("issues_spec.hrl").
-callback check_syntax(FileName::file:filename(), BaseFileName::file:filename(), Debug::boolean()) ->
    {ok, [warning() | error()]} | {error, [error()]}.
-callback output_error(ErrorInfo::error_info()) -> boolean().
-callback output_warning(ErrorInfo::error_info()) -> boolean().

-define(EXIT_SUCCESS, 0).
-define(EXIT_FAILURE, 1).
-type exit_code() :: ?EXIT_SUCCESS | ?EXIT_FAILURE.

%% ===================================================================
%% API
%% ===================================================================

-record(opts, {
    filename :: file:filename() | undefined,
    base :: file:filename() | undefined,
    debug = false :: boolean()
}).


-spec main([string()]) -> no_return().
main(Args) ->
    Opts = parse_args(Args, #opts{}),
    #opts{filename = FileName, base = BaseFileName, debug = Debug} = Opts,
    check_syntax(FileName, BaseFileName, Debug).

parse_args([], #opts{filename=undefined}) ->
    usage(?EXIT_FAILURE);
parse_args([], Opts = #opts{base=undefined, filename=FileName}) ->
    Opts#opts{base=FileName};
parse_args([], Opts) ->
    Opts;
parse_args([D | Args], Opts) when D =:= "-d"; D =:= "--debug" ->
    parse_args(Args, Opts#opts{debug=true});
parse_args([H | _Args], _Opts) when H =:= "-h"; H =:= "--help" ->
    usage(?EXIT_SUCCESS);
parse_args([B | Args], Opts) when B =:= "-b"; B =:= "--base" ->
    case {Args, Opts} of
        {[BaseFileName | Args0], #opts{base=undefined}} ->
            parse_args(Args0, Opts#opts{base=BaseFileName});
        {_Args, _Opts} ->
            usage(?EXIT_FAILURE)
    end;
parse_args([FileName | Args], Opts = #opts{filename=undefined}) ->
    parse_args(Args, Opts#opts{filename=FileName});
parse_args(["--", FileName | Args], Opts = #opts{filename=undefined}) ->
    parse_args(Args, Opts#opts{filename=FileName});
parse_args(_Args, _Opts) ->
    usage(?EXIT_FAILURE).

%% ===================================================================
%% Internal
%% ===================================================================

-spec check_syntax(string(), string(), boolean()) -> exit_code().
check_syntax(FileName, BaseFileName, Debug) ->
    ScriptName = escript:script_name(),
    HandlerPatterns = handler_patterns(ScriptName),
    syntaxerl_logger:debug(Debug, "Handler patterns: ~p", [HandlerPatterns]),
    Handler = choose_handler(FileName, HandlerPatterns),
    syntaxerl_logger:debug(Debug, "Selected handler: ~p", [Handler]),
    case Handler:check_syntax(FileName, BaseFileName, Debug) of
        {ok, Issues} ->
            syntaxerl_utils:print_issues(BaseFileName, FileName, Issues),
            halt(?EXIT_SUCCESS);
        {error, Issues} ->
            syntaxerl_utils:print_issues(BaseFileName, FileName, Issues),
            halt(?EXIT_FAILURE)
    end.

-spec usage(exit_code()) -> no_return().
usage(ExitCode) ->
    ScriptName = escript:script_name(),
    BaseName = filename:basename(ScriptName),
    case description_vsn(ScriptName) of
        {Description, Vsn} ->
            io:format("~s (~s)~n", [Description, Vsn]);
        _ ->
            io:format("~n")
    end,
    io:format("Usage: ~s [-b | --base <FILENAME>] [-d | --debug] <FILENAME>~n", [BaseName]),
    io:format("       ~s <-h | --help>~n", [BaseName]),
    io:format("  -b, --base     Set original filename~n"),
    io:format("  -d, --debug    Enable debug output~n"),
    io:format("  -h, --help     Show this message~n"),
    halt(ExitCode).

script_options(ScriptName) ->
    {ok, Sections} = escript:extract(ScriptName, []),
    Archive = proplists:get_value(archive, Sections),
    AppFile = lists:flatten(io_lib:format("~p/ebin/~p.app", [?MODULE, ?MODULE])),
    case zip:extract(Archive, [{file_list, [AppFile]}, memory]) of
        {ok, [{_, Binary}]} ->
            {ok, Tokens, _} = erl_scan:string(binary_to_list(Binary)),
            {ok, {application, ?MODULE, Options}} = erl_parse:parse_term(Tokens),
            Options;
        _ ->
            undefined
    end.

description_vsn(ScriptName) ->
    case script_options(ScriptName) of
        undefined ->
            undefined;
        Options ->
            Description = proplists:get_value(description, Options),
            Vsn = proplists:get_value(vsn, Options),
            {Description, Vsn}
    end.

handler_patterns(ScriptName) ->
    case script_options(ScriptName) of
        undefined ->
            undefined;
        Options ->
            proplists:get_value(handler_patterns, Options)
    end.

choose_handler(FileName, [{Patterns, Handler} | HandlerPatterns]) ->
    case match_patterns(FileName, Patterns) of
        match ->
            Handler;
        nomatch ->
            choose_handler(FileName, HandlerPatterns)
    end;
choose_handler(_FileName, []) ->
    erlang:error(no_handler_pattern_found).

match_patterns(FileName, [{suffix, Suffix} | Patterns]) ->
    case lists:suffix(Suffix, FileName) of
        true ->
            match_patterns(FileName, Patterns);
        false ->
            nomatch
    end;
match_patterns(FileName, [shebang | Patterns]) ->
    case read_first_byte(FileName) of
        {ok, <<"#">>} ->
            match_patterns(FileName, Patterns);
        _ ->
            nomatch
    end;
match_patterns(_FileName, []) ->
    match.

read_first_byte(FileName) ->
    case file:open(FileName, [read, binary]) of
        {ok, Fd} ->
            Result =
                case file:read(Fd, 1) of
                    {ok, Byte} ->
                        {ok, Byte};
                    _ ->
                        error
                end,
            file:close(Fd),
            Result;
        _ ->
            error
    end.
