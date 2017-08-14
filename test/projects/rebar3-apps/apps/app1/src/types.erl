-module(types).

-export_type([mydict/0]).

-ifdef(namespaced_types).
-type mydict() :: dict:dict().
-else.
-type mydict() :: dict().
-endif.
