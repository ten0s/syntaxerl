-ifndef(check_syntax_spec_hrl).
-define(check_syntax_spec_hrl, included).

-include("issues_spec.hrl").

-spec check_syntax(FileName::file:filename(), Debug::boolean()) ->
    {ok, [warning() | error()]} | {error, [error()]}.

-spec output_error(ErrorInfo::error_info()) -> boolean().

-spec output_warning(ErrorInfo::error_info()) -> boolean().

-endif.
