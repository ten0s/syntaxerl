-module(src).

-include_lib("lib1/include/lib1.hrl"). % _build/default/lib
-include_lib("lib2/include/lib2.hrl"). % _build/test/lib
-include_lib("lib3/include/lib3.hrl"). % _checkouts
-include_lib("app2/include/app2.hrl").
-include("ext.hrl").
-include("int.hrl").
