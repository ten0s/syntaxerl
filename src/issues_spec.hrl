-ifndef(issues_spec_hrl).
-define(issues_spec_hrl, included).

-type warning() :: {warning, Filename::file:filename(), Line::pos_integer(), Description::string()}.
-type error() :: {error, Filename::file:filename(), Description::string()} |
                 {error, Filename::file:filename(), Line::pos_integer(), Description::string()}.

-type issue() :: warning() | error().

-type line() :: none | pos_integer().

-type error_info() :: {
    ErrorLine::line(),
    Module::module(),
    ErrorDescriptor::term()
}.

-type error_list() :: [{FileName::file:filename(), [ErrorInfo::error_info()]}].
-type warning_list() :: error_list().

-endif.
