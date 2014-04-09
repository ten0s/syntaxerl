-ifndef(issues_spec_hrl).
-define(issues_spec_hrl, included).

-type warning() :: {warning, Line::integer(), Description::string()}.
-type error() :: {error, Description::string()} |
                 {error, Line::integer(), Description::string()}.

-type issue() :: warning() | error().

-endif.
