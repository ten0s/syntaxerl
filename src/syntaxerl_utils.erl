-module(syntaxerl_utils).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-export([
	incls_deps_opts/1,
	error_description/1,
	print_issues/2
]).

-include("issues_spec.hrl").

%% API

-spec incls_deps_opts(FileName::file:filename()) ->
	{InclDirs::[file:name()], EbinDirs::[file:name()], ErlcOpts::[term()]}.
incls_deps_opts(FileName) ->
	AbsFileName = filename:absname(FileName),
	BaseDir = filename:dirname(filename:dirname(AbsFileName)),

	StdOtpDirs = absdirs(BaseDir, ["./include", "./deps"]),
	StdErlcOpts = [
		strong_validation,

		{warn_format, 1},
		warn_export_vars,
		warn_shadow_vars,
		warn_obsolete_guard,
		warn_deprecated_function,
		warn_exported_vars,
		warn_bif_clash,
		warn_unused_import,
		warn_unused_function,
		warn_unused_variable,
		warn_unused_vars,
		warn_unused_record,

		return_errors,
		return_warnings
	],

	{DepsDirs, ErlcOpts} = deps_opts(BaseDir, StdOtpDirs, StdErlcOpts),

	{_, EbinDirs} = lists:mapfoldr(fun(Dir, Acc) -> {0, filelib:wildcard(Dir ++ "/*/ebin") ++ Acc} end, [], DepsDirs),
	IncludeDirs = lists:map(fun(Dir) -> {i, Dir} end, DepsDirs),
	{IncludeDirs, EbinDirs, ErlcOpts}.

-spec deps_opts(BaseDir::file:name(), OtpStdDirs::[file:name()], ErlcStdOpts::[term()]) -> {[file:name()], [term()]}.
deps_opts(BaseDir, OtpStdDirs, ErlcStdOpts) ->
	{DepsDirs, ErlcOpts} =
		case rebar_deps_opts(BaseDir) of
			{ok, {RebarDepsDirs, RebarErlcOpts}} ->
				{RebarDepsDirs, RebarErlcOpts};
			{error, bad_format} ->
				{[], []}; % nothing to do. fix your config file. :(
			{error, not_found} ->
				% sinan, Emakefile, ...
				{[], []}
		end,
	UniqDepsDirs = uniq(OtpStdDirs ++ DepsDirs),
	UniqErlcOpts = uniq(ErlcStdOpts ++ ErlcOpts),
	{UniqDepsDirs, UniqErlcOpts}.

%% the `file:format_error' returns the error description in the `line: description' format.
%% here only the `description' is returned.
-spec error_description({Line::integer(), Mod::module(), Term::term()}) -> string().
error_description(Error) ->
	tl(lists:dropwhile(fun(C) -> C =/= 32 end, file:format_error(Error))).

-spec print_issues(FileName::file:filename(), Issues::[issue()]) -> ok.
print_issues(_FileName, []) ->
	ok;
print_issues(FileName, [Issue | Issues]) ->
	print_issue(FileName, Issue),
	print_issues(FileName, Issues).

print_issue(FileName, {warning, Line, Description}) ->
	io:format("~s:~p: warning: ~s~n", [FileName, Line, Description]);
print_issue(FileName, {error, Description}) ->
	io:format("~s:~s~n", [FileName, Description]);
print_issue(FileName, {error, Line, Description}) ->
	io:format("~s:~p: ~s~n", [FileName, Line, Description]).

%% Internal

rebar_deps_opts("/") ->
	{error, not_found};
rebar_deps_opts(BaseDir) ->
	%% rebar specific begin
	RebarConfig = filename:join(BaseDir, "rebar.config"),
	%% rebar specific end
	case filelib:is_file(RebarConfig) of
		true ->
			case file:consult(RebarConfig) of
				{ok, Terms} ->
					%% rebar specific begin
					LibDirs = proplists:get_value(lib_dirs, Terms, []),
					DepsDir = proplists:get_value(deps_dir, Terms, "deps"),
					LocalDirs = LibDirs ++ [DepsDir],

					ErlcOpts = proplists:get_value(erl_opts, Terms, []),
					%% rebar specific end

					%% try to find recursively configs in parents directories.
					case rebar_deps_opts(filename:dirname(BaseDir)) of
						{ok, {ParentDirs, ParentErlcOpts}} ->
							{ok, {absdirs(BaseDir, uniq(LocalDirs ++ ParentDirs)), uniq(ErlcOpts ++ ParentErlcOpts)}};
						{error, _} ->
							{ok, {absdirs(BaseDir, uniq(LocalDirs)), ErlcOpts}}
					end;
				{error, _} ->
		            {error, bad_format}
			end;
		false ->
			rebar_deps_opts(filename:dirname(BaseDir))
	end.

uniq(List) ->
	sets:to_list(sets:from_list(List)).

absdirs(BaseDir, RelativeDirs) ->
	lists:map(fun(Dir) -> filename:join(BaseDir, Dir) end, RelativeDirs).
