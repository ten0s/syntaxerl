-ifndef(check_syntax_spec_hrl).
-define(check_syntax_spec_hrl, included).

-include("issues_spec.hrl").

-spec check_syntax(FileName::file:filename(), Debug::boolean()) -> {ok, [warning() | error()]}.

-endif.
