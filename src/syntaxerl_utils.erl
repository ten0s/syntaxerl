-module(syntaxerl_utils).
-author("Dmitry Klionsky <dm.klionsky@gmail.com>").

-export([
    incls_deps_opts/1,
    print_issues/2,
    consult_file/1
]).

-include("issues_spec.hrl").

-define(REBAR3DEPSDIRS, [
    "apps",
    "lib",
    "_checkouts",
    "_build/default/lib",
    "_build/test/lib"
]).

%% ===================================================================
%% API
%% ===================================================================

-spec incls_deps_opts(FileName::file:filename()) ->
    {InclDirs::[file:name()], EbinDirs::[file:name()], ErlcOpts::[term()]}.
incls_deps_opts(FileName) ->
    AbsFileName = filename:absname(FileName),
    AppDir = appdir(AbsFileName),
    ProjectDir = projectdir(AppDir),
    StdOtpDirs = include_dirs(AbsFileName, AppDir, ProjectDir),
    StdErlcOpts = [
        strong_validation,

        {warn_format, 1},
        warn_export_vars,
        warn_shadow_vars,
        warn_obsolete_guard,
        warn_deprecated_function,
        warn_exported_vars,
        warn_bif_clash,
        warn_unused_function,
        warn_unused_variable,
        warn_unused_vars,
        warn_unused_record,

        return_errors,
        return_warnings
    ],
    Profile = which_compile_opts_profile(AbsFileName),
    {DepsDirs, ErlcOpts} = deps_opts(AppDir, StdOtpDirs, StdErlcOpts, Profile),

    ok = add_deps_paths(StdOtpDirs),

    {_, EbinDirs} = lists:mapfoldr(fun(Dir, Acc) -> {0, filelib:wildcard(Dir ++ "/*/ebin") ++ Acc} end, [], DepsDirs),
    IncDirs = lists:map(fun(Dir) -> {i, Dir} end, DepsDirs),
    {IncDirs, EbinDirs, ErlcOpts}.

-spec appdir(AbsFileName::file:name()) -> file:name().
appdir(AbsFileName) ->
    appdir(AbsFileName, filename:dirname(AbsFileName)).

-spec appdir(AbsFileName::file:name(), Default::file:name()) -> file:name().
appdir(AbsFileName, Default) ->
    AbsFileDir = filename:dirname(AbsFileName),
    case filename:basename(AbsFileDir) of
        Dir when Dir =:= "src"; Dir =:= "test"; Dir =:= "include" ->
            filename:dirname(AbsFileDir);
        Dir when Dir =/= "" ->
            appdir(AbsFileDir, Default);
        _Dir ->
            Default
    end.

-spec projectdir(Dir::file:name()) -> file:name().
projectdir(Dir) ->
    PDir = filename:dirname(Dir),
    case filename:basename(PDir) of
        "apps" ->
            filename:dirname(PDir);
        "deps" ->
            filename:dirname(PDir);
        "lib" ->
            BuildDir = filename:dirname(filename:dirname(PDir)),
            case filename:basename(BuildDir) of
                "_build" -> filename:dirname(BuildDir);
                _ -> Dir
            end;
        _Dir ->
            Dir
    end.

-spec include_dirs(AbsFileName, AppDir, ProjectDir) -> Dirs::[file:name()]
    when AbsFileName::file:name(), AppDir::file:name(), ProjectDir::file:name().
include_dirs(AbsFileName, AppDir, ProjectDir) ->
    SrcDir = filename:dirname(AbsFileName),
    IncludeDir = filename:join(AppDir, "include"),
    BuildDepsDir = filename:join(ProjectDir, "deps"),
    DepsDirs = [ filename:join(ProjectDir, Dir) || Dir <- ?REBAR3DEPSDIRS ],
    [SrcDir, IncludeDir, BuildDepsDir | DepsDirs].

-spec add_deps_paths(Dir::[file:name()]) -> ok.
add_deps_paths(Dirs) ->
    lists:foreach(fun (Dir) ->
        case file:list_dir(Dir) of
            {ok, Deps} ->
                lists:foreach(fun (Dep) ->
                    EBin = filename:join([Dir, Dep, "ebin"]),
                    case filelib:is_dir(EBin) of
                        true -> code:add_path(EBin);
                        false -> ok
                    end
                end, Deps);
            {error, _Error} ->
                ok
        end
    end, Dirs).

-spec deps_opts(BaseDir::file:name(), OtpStdDirs::[file:name()], ErlcStdOpts::[term()], normal | test) -> {[file:name()], [term()]}.
deps_opts(BaseDir, OtpStdDirs, ErlcStdOpts, Profile) ->
    {DepsDirs, ErlcOpts} =
        case find_deps_opts(BaseDir, Profile) of
            {ok, {RebarDepsDirs, RebarErlcOpts}} ->
                {RebarDepsDirs, RebarErlcOpts};
            {error, bad_format} ->
                {[], []}; % nothing to do. fix your config file. :(
            {error, {erlangmk, _}} ->
                {[], []}; % Failure with make(1) or the Makefile.
            {error, not_found} ->
                % sinan, Emakefile, ...
                {[], []}
        end,
    UniqDepsDirs = uniq(OtpStdDirs ++ DepsDirs),
    UniqErlcOpts = uniq(ErlcStdOpts ++ ErlcOpts),
    {UniqDepsDirs, UniqErlcOpts}.

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

-spec consult_file(file:filename()) -> {ok, term()} | {error, term()}.
consult_file(File) ->
    case filename:extension(File) of
        ".script" ->
            consult_and_eval(remove_script_ext(File), File);
        _ ->
            Script = File ++ ".script",
            case filelib:is_regular(Script) of
                true ->
                    consult_and_eval(File, Script);
                false ->
                    file:consult(File)
            end
    end.


%% ===================================================================
%% API
%% ===================================================================

find_deps_opts(BaseDir, Profile) ->
    case which_build_tool(BaseDir) of
        {rebar, NewBaseDir} ->
            rebar_deps_opts(NewBaseDir);
        {erlangmk, NewBaseDir} ->
            erlangmk_deps_opts(NewBaseDir, Profile);
        undefined ->
            {error, not_found}
    end.

which_compile_opts_profile(File) ->
    case filename:basename(filename:dirname(File)) of
        "test" -> test;
        _      -> normal
    end.

which_build_tool("/") ->
    undefined;
which_build_tool(BaseDir) ->
    RebarConfig = filename:join(BaseDir, "rebar.config"),
    case filelib:is_file(RebarConfig) of
        true ->
            {rebar, BaseDir};
        false ->
            ErlangMk = filename:join(BaseDir, "erlang.mk"),
            case filelib:is_file(ErlangMk) of
                true ->
                    {erlangmk, BaseDir};
                false ->
                    which_build_tool(filename:dirname(BaseDir))
            end
    end.

rebar_deps_opts("/") ->
    {error, not_found};
rebar_deps_opts(BaseDir) ->
    RebarConfig = filename:join(BaseDir, "rebar.config"),
    case filelib:is_file(RebarConfig) of
        true ->
            case consult_file(RebarConfig) of
                {ok, Terms} ->
                    ErlcOpts = rebar_erl_opts(proplists:get_value(erl_opts, Terms, [])),

                    RebarLibDirs = proplists:get_value(lib_dirs, Terms, []),
                    RebarDepsDir = proplists:get_value(deps_dir, Terms, "deps"),
                    LocalDirs = RebarLibDirs
                             ++ [RebarDepsDir]
                             ++ ?REBAR3DEPSDIRS
                             ++ proplists:get_all_values(i, ErlcOpts),

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

-spec rebar_erl_opts(Opts) -> Opts when Opts :: [term()].
rebar_erl_opts(ErlOpts) ->
    rebar_erl_opts(ErlOpts, []).

-spec rebar_erl_opts(Opts, Opts) -> Opts when Opts :: [term()].
rebar_erl_opts([], Acc) ->
    lists:reverse(Acc);
rebar_erl_opts([{platform_define, ArchRegex, Key} | ErlOpts], Acc) ->
    rebar_erl_opts(ErlOpts,
        case is_arch(ArchRegex) of
            true -> [{d, Key} | Acc];
            false -> Acc
        end);
rebar_erl_opts([{platform_define, ArchRegex, Key, Value} | ErlOpts], Acc) ->
    rebar_erl_opts(ErlOpts,
        case is_arch(ArchRegex) of
            true -> [{d, Key, Value} | Acc];
            false -> Acc
        end);
rebar_erl_opts([ErlOpt | ErlOpts], Acc) ->
    rebar_erl_opts(ErlOpts, [ErlOpt | Acc]).

-spec is_arch(ArchRegex :: string()) -> boolean().
is_arch(ArchRegex) ->
    re:run(get_arch(), ArchRegex, [{capture, none}]) =:= match.

-spec get_arch() -> string().
get_arch() ->
    erlang:system_info(otp_release) ++ "-"
        ++ erlang:system_info(system_architecture) ++ "-"
        ++ wordsize().

-spec wordsize() -> string().
wordsize() ->
    try erlang:system_info({wordsize, external}) of
        Val ->
            integer_to_list(8 * Val)
    catch
        error:badarg ->
            integer_to_list(8 * erlang:system_info(wordsize))
    end.

erlangmk_deps_opts(BaseDir, Profile) ->
    Make =
        case os:getenv("MAKE") of
            false ->
                case os:find_executable("gmake") of
                    false -> "make";
                    Path  -> Path
                end;
            Cmd ->
                case (lists:member($/, Cmd) orelse lists:member($\\, Cmd)) of
                    true  -> Cmd;
                    false -> os:find_executable(Cmd)
                end
        end,
    ERLC_OPTS_Target =
        case Profile of
            normal -> "show-ERLC_OPTS";
            test   -> "show-TEST_ERLC_OPTS"
        end,
    Args = [
        "--no-print-directory",
        "-C", BaseDir,
        "show-ERL_LIBS",
        ERLC_OPTS_Target
    ],
    try
        Port = erlang:open_port({spawn_executable, Make}, [
            {args, Args},
            exit_status, use_stdio, stderr_to_stdout]),
        erlangmk_port_receive_loop(Port, "", BaseDir)
    catch
        error:_ ->
            {error, {erlangmk, make_execution_failure}}
    end.

erlangmk_port_receive_loop(Port, Stdout, BaseDir) ->
    receive
        {Port, {exit_status, 0}} ->
            erlangmk_format_opts(Stdout, BaseDir);
        {Port, {exit_status, _}} ->
            {error, {erlangmk, make_target_failure}};
        {Port, {data, Out}} ->
            erlangmk_port_receive_loop(Port, Stdout ++ Out, BaseDir)
    end.

erlangmk_format_opts(Stdout, BaseDir) ->
    case string:tokens(Stdout, "\n") of
        [ErlLibsLine | ErlcOptsLines] ->
            ErlLibs = erlangmk_format_erl_libs(ErlLibsLine),
            ErlcOpts = erlangmk_format_erlc_opts(ErlcOptsLines, BaseDir),
            {ok, {ErlLibs, ErlcOpts}};
        _ ->
            {error, {erlangmk, incorrect_output}}
    end.

erlangmk_format_erl_libs(ErlLibsLine) ->
    case os:type() of
        {win32, _} -> string:tokens(ErlLibsLine, ";");
        _          -> string:tokens(ErlLibsLine, ":")
    end.

erlangmk_format_erlc_opts(ErlcOptsLines, BaseDir) ->
    erlangmk_format_erlc_opts(ErlcOptsLines, [], BaseDir).

erlangmk_format_erlc_opts(["+" ++ Option | Rest], Opts, BaseDir) ->
    case make_term(Option) of
        {error, _} -> erlangmk_format_erlc_opts(Rest, Opts, BaseDir);
        Opt        -> erlangmk_format_erlc_opts(Rest, [Opt | Opts], BaseDir)
    end;
erlangmk_format_erlc_opts(["-I" ++ Opt | Rest], Opts, BaseDir)
  when Opt =/= "" ->
    erlangmk_format_erlc_opts(["-I", Opt | Rest], Opts, BaseDir);
erlangmk_format_erlc_opts(["-I", [C | _] = Dir | Rest], Opts, BaseDir)
  when C =/= $- andalso C =/= $+ ->
    AbsDir = filename:absname(Dir, BaseDir),
    erlangmk_format_erlc_opts(Rest, [{i, AbsDir} | Opts], BaseDir);
erlangmk_format_erlc_opts(["-W" ++ Warn | Rest], Opts, BaseDir)
  when Warn =/= "" ->
    erlangmk_format_erlc_opts(["-W", Warn | Rest], Opts, BaseDir);
erlangmk_format_erlc_opts(["-W", Warn | Rest], Opts, BaseDir) ->
    case Warn of
        "all" ->
            erlangmk_format_erlc_opts(Rest, [{warn_format, 999} | Opts],
                BaseDir);
        "error" ->
            erlangmk_format_erlc_opts(Rest, [warnings_as_errors | Opts],
                BaseDir);
        "" ->
            erlangmk_format_erlc_opts(Rest, [{warn_format, 1} | Opts],
                BaseDir);
        _ ->
            try list_to_integer(Warn) of
                Level ->
                    erlangmk_format_erlc_opts(Rest,
                        [{warn_format, Level} | Opts], BaseDir)
            catch
                error:badarg ->
                    erlangmk_format_erlc_opts(Rest, Opts, BaseDir)
            end
    end;
erlangmk_format_erlc_opts(["-D" ++ Opt | Rest], Opts, BaseDir)
  when Opt =/= "" ->
    erlangmk_format_erlc_opts(["-D", Opt | Rest], Opts, BaseDir);
erlangmk_format_erlc_opts(["-D", [C | _] = Val0 | Rest], Opts, BaseDir)
  when C =/= $- andalso C =/= $+ ->
    {Key0, Val1} = split_at_equals(Val0, []),
    Key = list_to_atom(Key0),
    case Val1 of
        [] ->
            erlangmk_format_erlc_opts(Rest, [{d, Key} | Opts], BaseDir);
        Val2 ->
            case make_term(Val2) of
                {error, _} ->
                    erlangmk_format_erlc_opts(Rest, Opts, BaseDir);
                Val ->
                    erlangmk_format_erlc_opts(Rest, [{d, Key, Val} | Opts], BaseDir)
            end
    end;
erlangmk_format_erlc_opts([PathFlag, [_ | _] = Dir | Rest], Opts, BaseDir)
  when PathFlag =:= "-pa" orelse PathFlag =:= "-pz" ->
    AbsDir = filename:absname(Dir, BaseDir),
    case PathFlag of
        "-pa" -> code:add_patha(AbsDir);
        "-pz" -> code:add_pathz(AbsDir)
    end,
    erlangmk_format_erlc_opts(Rest, Opts, BaseDir);
erlangmk_format_erlc_opts([_ | Rest], Opts, BaseDir) ->
    erlangmk_format_erlc_opts(Rest, Opts, BaseDir);
erlangmk_format_erlc_opts([], Opts, _) ->
    lists:reverse(Opts).

%% Function imported from erl_compile.erl from Erlang 19.1.
make_term(Str) ->
    case erl_scan:string(Str) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
                {ok, Term}      -> Term;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason, _} ->
            {error, Reason}
    end.

%% Function imported from erl_compile.erl from Erlang 19.1.
split_at_equals([$=|T], Acc) ->
    {lists:reverse(Acc),T};
split_at_equals([H|T], Acc) ->
    split_at_equals(T, [H|Acc]);
split_at_equals([], Acc) ->
    {lists:reverse(Acc),[]}.

consult_and_eval(File, Script) ->
    ConfigData = try_consult(File),
    file:script(Script, bs([{'CONFIG', ConfigData}, {'SCRIPT', Script}])).

remove_script_ext(F) ->
    "tpircs." ++ Rev = lists:reverse(F),
    lists:reverse(Rev).

try_consult(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            Terms;
        {error, enoent} ->
            [];
        {error, Reason} ->
            syntaxerl_logger:debug(true, "Failed to read config file ~s: ~p~n", [File, Reason])
    end.

bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                    erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

uniq(List) ->
    sets:to_list(sets:from_list(List)).

absdirs(BaseDir, RelativeDirs) ->
    lists:map(fun(Dir) -> filename:join(BaseDir, Dir) end, RelativeDirs).
