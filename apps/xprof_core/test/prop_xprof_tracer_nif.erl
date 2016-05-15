-module(prop_xprof_tracer_nif).

-include_lib("proper/include/proper.hrl").

%%-define(MOD, erl_tracer).
-define(MOD, xprof_tracer_nif).

%% Note that proper does not support a number of built in types
%% (pid(), port(), module(), map())
%% so need to use generators instead.

module() ->
    atom().

pid(Pids) ->
    union(Pids).

%% -type tracee() :: port() | pid() | undefined.
tracee(Pids) ->
    weighted_union([{9, pid(Pids)},
                    {1, undefined}]).

-type trace_tag_running_ports() :: in | out | in_exiting | out_exiting | out_exited.
-type trace_tag_running_procs() :: in | out | in_exiting | out_exiting | out_exited.
-type trace_tag_send() :: send | send_to_non_existing_process.
-type trace_tag_receive() :: 'receive'.
-type trace_tag_call() :: call | return_to | return_from | exception_from.
-type trace_tag_procs() :: spawn | spawned | exit | link | unlink
                         | getting_linked | getting_unlinked
                         | register | unregister.
-type trace_tag_ports() :: open | closed  | link | unlink
                         | getting_linked | getting_unlinked.
-type trace_tag_gc() :: gc_minor_start | gc_minor_end
                      | gc_major_start | gc_major_end.

-type trace_tag() :: trace_tag_send()
                   | trace_tag_receive()
                   | trace_tag_call()
                   | trace_tag_procs()
                   | trace_tag_ports()
                   | trace_tag_running_procs()
                   | trace_tag_running_ports()
                   | trace_tag_gc().

%%-type trace_opts() :: #{ extra => term(), match_spec_result => term(),
%%                         scheduler_id => non_neg_integer(),
%%                         timestamp => timestamp | cpu_timestamp |
%%                                      monotonic | strict_monotonic }.
-type opt() ::
        {extra, term()} |
        {match_spec_result, arity | term()} |
        {scheduler_id, non_neg_integer()} |
        {timestamp, timestamp | cpu_timestamp |
                    monotonic | strict_monotonic}.

 -type trace_opts() ::
         list(opt()).

%% -type tracer_state() :: pid().
tracer_state(TracerPid) ->
    TracerPid.

prop_enabled_trace_status() ->
    Pids = create_pids(),
    TracerPid = hd(Pids),
    ?FORALL({TracerState, Tracee},
            {tracer_state(TracerPid),
             tracee(Pids)},
            begin
                Result = ?MOD:enabled(
                           trace_status, TracerState, Tracee),
                cleanup_pids(Pids),
                true =:= lists:member(Result, [trace, remove]) orelse throw(Result)
            end).

prop_enabled() ->
    Pids = create_pids(),
    TracerPid = hd(Pids),
    ?FORALL({TraceTag, TracerState, Tracee},
            {union([trace_tag(), seq_trace]),
             tracer_state(TracerPid),
             tracee(Pids)},
            begin
                Result = ?MOD:enabled(
                           TraceTag, TracerState, Tracee),
                cleanup_pids(Pids),
                true =:= lists:member(Result, [trace, discard]) orelse throw(Result)
            end).

prop_trace() ->
    Pids = create_pids(),
    TracerPid = hd(Pids),
    ?FORALL({TraceTag, TracerState, Tracee, Msg, Opts} = _Args,
            {union([trace_tag(), seq_trace]),
             tracer_state(TracerPid),
             tracee(Pids),
             {module(), atom(), list()}, %% MFA
             trace_opts()},
            begin
                %% io:format(user, "case ~p~n", [_Args]),
                _Any =
                    ?MOD:trace(
                       TraceTag, TracerState, Tracee,
                       Msg, maps:from_list(Opts)),
                cleanup_pids(Pids),
                %% if it hasn't crashed the VM so far it's fine
                true
            end).



create_pids() ->
    Pids = [_TracerPid, _AlivePid, DeadPid] =
        [spawn_link(fun() -> receive stop -> stop end end)
         || _ <- [tracer_pid, alive_pid, dead_pid]],
    kill_pid(DeadPid),
    Pids.

cleanup_pids(Pids) ->
    [kill_pid(Pid) || Pid <- Pids],
    ok.

kill_pid(Pid) ->
    MRef = monitor(process, Pid),
    unlink(Pid),
    exit(Pid, kill),
    receive
        {'DOWN', MRef, process, Pid, _} ->
            ok
    after 5000 ->
            exit(nostop)
    end.
