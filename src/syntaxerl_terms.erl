-module(syntaxerl_terms).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-behaviour(syntaxerl).
-export([check_syntax/2]).

-include("check_syntax_spec.hrl").

check_syntax(FileName, _Debug) ->
	case file:eval(FileName) of
		ok ->
			{ok, []};
		{error, Error} ->
			%% unfortunately the `file:eval' returns only the first error.
			{ok, [{error, file:format_error(Error)}]}
	end.
