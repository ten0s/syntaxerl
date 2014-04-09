-module(syntaxerl).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").
-export([behaviour_info/1]).
-export([main/1]).

-spec behaviour_info(callbacks | any()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
    [{check_syntax, 2}];
behaviour_info(_) ->
    undefined.

%% ===================================================================
%% API
%% ===================================================================

-spec main([string()]) -> no_return().
main([""]) ->
    usage();
main(["-h"]) ->
    usage();
main(["--help"]) ->
    usage();
main(["-d"]) ->
    usage();
main(["--debug"]) ->
    usage();
main([FileName]) ->
    check_syntax(FileName, false);
main([FileName, "-d"]) ->
    check_syntax(FileName, true);
main(["-d", FileName]) ->
    check_syntax(FileName, true);
main([FileName, "--debug"]) ->
    check_syntax(FileName, true);
main(["--debug", FileName]) ->
    check_syntax(FileName, true);
main(_) ->
    usage().

%% ===================================================================
%% Internal
%% ===================================================================

check_syntax(FileName, Debug) ->
    ScriptName = escript:script_name(),
    Handlers = handlers(ScriptName),
    syntaxerl_logger:debug(Debug, "Handlers: ~p", [Handlers]),
    Handler = get_handler(FileName, Handlers),
    syntaxerl_logger:debug(Debug, "Selected handler: ~p", [Handler]),
    case Handler:check_syntax(FileName, Debug) of
        {ok, Issues} ->
            syntaxerl_utils:print_issues(FileName, Issues);
        {error, Issues} ->
            syntaxerl_utils:print_issues(FileName, Issues)
    end.

usage() ->
    ScriptName = escript:script_name(),
    BaseName = filename:basename(ScriptName),
    io:format("Usage: ~s [-d] filename~n", [BaseName]),
    io:format("Usage: ~s [-h]~n", [BaseName]),
    case description_vsn(ScriptName) of
        {Description, Vsn} ->
            io:format("~s (~s)~n~n", [Description, Vsn]);
        _ ->
            io:format("~n")
    end,
    io:format("  -d, --debug    Enable debug output~n"),
    io:format("  -h, --help     Show this message~n~n").

script_options(ScriptName) ->
    {ok, Sections} = escript:extract(ScriptName, []),
    ZipArchive = proplists:get_value(archive, Sections),
    AppName = lists:flatten(io_lib:format("~p.app", [?MODULE])),
    case zip:extract(ZipArchive, [{file_list, [AppName]}, memory]) of
        {ok, [{AppName, Binary}]} ->
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

handlers(ScriptName) ->
    case script_options(ScriptName) of
        undefined ->
            undefined;
        Options ->
            proplists:get_value(handlers, Options)
    end.

get_handler(FileName, [{Pattern, Handler} | Patterns]) ->
    case re:run(FileName, Pattern, [{capture, none}]) of
        match ->
            Handler;
        nomatch ->
            get_handler(FileName, Patterns)
    end.
