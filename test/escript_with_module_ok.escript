#!/usr/bin/env escript

-module(escript_with_module_ok).

-export([main/1]).

-spec main(list()) -> no_return().
main([]) ->
    no_arg;
main([arg]) ->
    arg.
