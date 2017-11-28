%%% Taken from buoy_profile
-module(xprof_profile).

-export([fprofx/0,

         test_fun/0
        ]).

-define(N, 1000).
-define(P, 20).

-define(TRACE_FILE, "fprofx.trace").

%% public
-spec fprofx() -> ok.

fprofx() ->
    Filenames = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
    Rootnames = [filename:rootname(Filename, ".beam") ||
        Filename <- Filenames],
    lists:foreach(fun code:load_abs/1, Rootnames),

    %% Prepare

    MFA = {?MODULE, test_fun, 0},

    Parent = self(),
    %% has to be started before xprofx so it is excluded from the `new` procs
    %% scope
    Workers = 100,
    Pid = spawn(fun() -> work(Parent, Workers) end),

    %% this clears all trace_patterns :(
    {ok, StartedApps} = xprof:start(),

    %% setup trace_patterns for fprof but don't yet start tracing any process
    setup_fprof_patterns(),

    %% Start/Init

    xprof_core:trace(Pid),
    ok = xprof_core:monitor(MFA),

    XprofTracer = whereis(xprof_core_tracer),
    [{_, XprofTraceHandler,_,_}] =
        supervisor:which_children(xprof_core_trace_handler_sup),
    XPids = [XprofTracer, XprofTraceHandler],
    Tracer = start_fprof_trace(XPids),

    %% Generate work

    io:format("STARTING WORK ~p~n", [os:timestamp()]),
    start_work(Pid),
    wait_for_work(Pid),
    io:format("WORK FINISHED~p~n", [os:timestamp()]),
    %% a snapshot of the data collected in hdr_histogram is taken only once a
    %% second
    timer:sleep(1000),

    Values = xprof_core:get_data(MFA, 0),
    io:format("collected data~n~p~n", [Values]),

    %% Stop tracing

    stop_fprof_trace(Tracer, XPids),

    xprof_core:demonitor(MFA),
    lists:foreach(fun application:stop/1, StartedApps),

    ok = fprofx:profile(),
    ok = fprofx:analyse([totals, {dest, ""}, {cols,80}]),
    ok = fprofx:analyse([totals,
                         {dest, "fprofx.ana.own"},
                         {cols, 80},
                         {details, false},
                         {sort, acc},
                         {callers, false}]),
    fprofx:stop(),

    ok.

%% Trace sent to FprofX

setup_fprof_patterns() ->
    MatchSpec = [{'_', [], [{message, {{cp, {caller}}}}]}],
    erlang:trace_pattern(on_load, MatchSpec, [local]),
    erlang:trace_pattern({'_', '_', '_'}, MatchSpec, [local]),
    %% Don't trace this module - xprof will set it's own trace pattern for the
    %% monitored `?MODULE:test_fun/1' only. Otherwise `xprof_tracer` would
    %% receive trace message about `loop/1` etc called.
    erlang:trace_pattern({?MODULE, '_', '_'}, false, [local]),
    ok.

%% modified version of fprof:trace_on/3
start_fprof_trace(Procs) ->
    Tracer = open_dbg_trace_port(),
    lists:foreach(
      fun(P) ->
		      erlang:trace(P, true, [{tracer, Tracer} | trace_flags()])
      end,
      Procs),
    Tracer.

open_dbg_trace_port() ->
    Fun = dbg:trace_port(file, ?TRACE_FILE),
    Fun().

trace_flags() ->
    [call, return_to,
     running, procs, garbage_collection,
     arity, timestamp, set_on_spawn].

%% modified version of fprof:trace_off/0
stop_fprof_trace(Tracer, Procs) ->
    %% only stop tracing of the profiled processes
    [try erlang:trace_delivered(P) of
         Ref ->
             receive {trace_delivered, P, Ref} ->
                     erlang:trace(P, false, [all])
             end
     catch
         error:undef -> ok
     end
     || P <- Procs],

    %% also stop the tracer port that writes to file
    catch erlang:port_close(Tracer),

    ok.

%% Work profiled by XProf
start_work(Pid) ->
    Pid ! {self(), start}.

wait_for_work(Pid) ->
    receive
        {Pid, done} ->
            ok
    after 5000 ->
            exit(timeout_work)
    end.

work(Parent, N) ->
    receive
        {Parent, start} ->
            loop(N),
            Parent ! {self(), done}
    after 5000 ->
            io:format("timed out waiting for start ~p~n", [self()]),
            exit(timeout_start)
    end.

loop(0) ->
    ok;
loop(N) ->
    test_fun(),
    loop(N - 1).

test_fun() ->
    3+3.
