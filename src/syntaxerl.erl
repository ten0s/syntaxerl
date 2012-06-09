-module(syntaxerl).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").
-export([behaviour_info/1]).
-export([main/1]).

-spec behaviour_info(callbacks | any()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
	[{check_syntax, 2}];
behaviour_info(_) ->
	undefined.

-spec main([string()]) -> no_return().
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

check_syntax(FileName, Debug) ->
	Handlers = [
		{".erl", syntaxerl_erl},
		{".hrl", syntaxerl_hrl}
	],
	Ext = filename:extension(FileName),
	Handler = proplists:get_value(Ext, Handlers, syntaxerl_terms),
	syntaxerl_logger:debug(Debug, "Selected handler: ~p", [Handler]),
	case Handler:check_syntax(FileName, Debug) of
		{ok, Issues} ->
			syntaxerl_utils:print_issues(FileName, Issues);
		{error, Issues} ->
			syntaxerl_utils:print_issues(FileName, Issues)
	end.

usage() ->
	ScriptName = escript:script_name(),
    io:format("Usage: ~s [-d] filename~n", [filename:basename(ScriptName)]),
	case description_vsn(ScriptName) of
		{Description, Vsn} ->
			io:format("~s (~s)~n~n", [Description, Vsn]);
		_ ->
			io:format("~n")
	end,
	io:format("  -d, --debug    Enable debug output~n~n"),
    halt(1).

description_vsn(ScriptName) ->
	{ok, Sections} = escript:extract(ScriptName, []),
	ZipArchive = proplists:get_value(archive, Sections),
	AppName = lists:flatten(io_lib:format("~p.app", [?MODULE])),
	case zip:extract(ZipArchive, [{file_list, [AppName]}, memory]) of
		{ok, [{AppName, Binary}]} ->
			{ok, Tokens, _} = erl_scan:string(binary_to_list(Binary)),
		    {ok, {application, ?MODULE, Options}} = erl_parse:parse_term(Tokens),
			Description = proplists:get_value(description, Options),
			Vsn = proplists:get_value(vsn, Options),
			{Description, Vsn};
		_ ->
			undefined
	end.
