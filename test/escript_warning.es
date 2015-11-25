#!/usr/bin/env escript

-export([main/1]).

main([]) ->
    no_arg;
main([arg]) ->
    arg.
