-module(syntaxerl_logger).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").
-export([debug/3]).

-spec debug(Output::boolean(), Fmt::string(), Args::[term()]) -> ok.
debug(true, Fmt, Args) ->
	io:format("DEBUG: " ++ Fmt ++ "~n", Args);
debug(false, _Fmt, _Args) ->
	ok.


