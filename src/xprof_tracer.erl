%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(xprof_tracer).

-behaviour(gen_server).

-export([start_link/1,
         trace/1,
         monitor/1, demonitor/1,
         all_monitored/0,
         trace_status/0,
         data/2]).

%% gen_server callbacks

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("xprof.hrl").

-record(state, {trace_spec  = all,   %% trace specification
                paused      = true,  %% tracing paused?
                overflow    = false, %% tracing is paused because of overflow?
                funs        = [],    %% functions monitored by xprof
                node                 %% node to trace on
               }).

%% @doc Starts xprof tracer process.
-spec start_link(node()) -> {ok, pid()}.
start_link(Node) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Node], []).

%% @doc Starts monitoring specified function calls.
-spec monitor(mfa()) -> ok.
monitor({Mod, Fun, Arity} = MFA) ->
    lager:info("Starting monitoring ~w:~w/~b",[Mod,Fun,Arity]),
    gen_server:call(?MODULE, {monitor, MFA}).

%% @doc Stops monitoring specified function calls.
-spec demonitor(mfa()) -> ok.
demonitor({Mod, Fun, Arity} = MFA) ->
    lager:info("Stopping monitoring ~w:~w/~b",[Mod,Fun,Arity]),
    gen_server:call(?MODULE, {demonitor, MFA}).

%% @doc Returns list of monitored functions
-spec all_monitored() -> list(mfa()).
all_monitored() ->
    gen_server:call(?MODULE, all_monitored).

%% @doc Returns metrics gathered for particular function.
-spec data(mfa(), non_neg_integer()) -> list(proplists:proplist()) |
                                        {error, not_found}.
data(MFA, TS) ->
    xprof_tracer_handler:data(MFA, TS).

%% @doc Turns on or resumes tracing for a process specified by pid, all
%% processes or processes that are spawned by specified spawner pid.
-spec trace(pid() | pause| resume | all | {spawner, pid()}) -> ok.
trace(PidOrSpec) ->
    lager:info("Tracing ~p", [PidOrSpec]),
    gen_server:call(?MODULE, {trace, PidOrSpec}).

%% @doc Returns current tracing state.
-spec trace_status() -> {all | {spawner, pid(), float()} | pid(),
                         Paused :: boolean(), Overflow :: boolean()}.
trace_status() ->
    gen_server:call(?MODULE, trace_status).


%% gen_server callbacks

init([Node]) ->
    init_tracer(Node),
    {ok, #state{node = Node}}.

handle_call({monitor, MFA}, _From, State) ->
    case get({handler, MFA}) of
        Pid when is_pid(Pid) ->
            {reply, {error, already_traced}, State};
        undefined ->
            {ok, Pid} = supervisor:start_child(xprof_tracer_handler_sup, [MFA]),
            put({handler, MFA}, Pid),

            MatchSpec = [{'_', [], [{return_trace}]}],
            trace_pattern(State#state.node, MFA, MatchSpec, [local]),

            {reply, ok, State#state{funs=State#state.funs ++ [MFA]}}
    end;
handle_call({demonitor, MFA}, _From, State) ->
    trace_pattern(State#state.node, MFA, false, [local]),

    Pid = erase({handler, MFA}),
    NewFuns = lists:filter(fun(E) -> E =/= MFA end, State#state.funs),

    supervisor:terminate_child(xprof_tracer_handler_sup, Pid),
    {reply, ok, State#state{funs=NewFuns}};
handle_call(all_monitored, _From, State = #state{funs=MFAs}) ->
    {reply, MFAs, State};
handle_call({trace, PidSpec}, _From, State) ->
    NewState = setup_trace(PidSpec, State),
    {reply, ok, NewState};
handle_call(trace_status, _From, State = #state{trace_spec=TraceSpec,
                                                paused=Paused,
                                                overflow=Overflow}) ->
    {reply, {TraceSpec, Paused, Overflow}, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg = {trace_ts, _TracedPid, call, {M,F,Args}, _StartTime}, State) ->
    NewState = check_for_overflow(State),
    MFA = {M,F,length(Args)},
    case get({handler, MFA}) of
        undefined ->
            ok;
        Pid ->
            erlang:send(Pid, Msg)
    end,
    {noreply, NewState};
handle_info(Msg = {trace_ts, _TracedPid, return_from, MFA , _Res, _StartTime},
            State) ->
    NewState = check_for_overflow(State),
    case get({handler, MFA}) of
        undefined ->
            {noreply, NewState};
        Pid ->
            erlang:send(Pid, Msg),
            {noreply, NewState}
    end;
handle_info({trace_ts, _Spawner, spawn, NewProc, _MFArgs,_TimeStamp},
            State = #state{trace_spec=TraceSpec}) ->
    NewState = check_for_overflow(State),

    %% trace spec could have been changed while there were late messages
    %% in the queue
    Sampl = case TraceSpec of
                {spawner, _Pid, X} -> X;
                _ -> 0.0
            end,

    case random:uniform() < Sampl of
        true ->
            catch erlang:trace(NewProc, true, [call, procs, timestamp]);
        false ->
            ok
    end,
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

init_tracer(Node) when Node =:= node() ->
    erlang:trace_pattern({'_','_','_'}, false, [local]),
    erlang:trace(all, true, [call, timestamp]);
init_tracer(Node) ->
    %% tcp port of tracer server
    Port = application:get_env(?APP, trace_srv_port, ?DEF_TRACE_SRV_PORT),
    Parent = self(),
    %% start tracer on remote node
    netload(Node, ?MODULE),
    netload(Node, xprof_vm_info),
    RemotePid =
        spawn_link(
          Node,
          fun() ->
                  erlang:trace_pattern({'_','_','_'}, false, [local]),
                  Tracer = (dbg:trace_port(ip, Port))(),
                  erlang:trace(all, true, [call, timestamp, {tracer, Tracer}]),
                  Parent ! {self(), started},
                  receive
                      {Parent, stop} ->
                          Parent ! {self(), stopped}
                  end
          end),
    receive {RemotePid, started} -> ok end,
    %% start trace client on local node
    dbg:trace_client(ip, Port,
                     {fun({drop, _}, S) -> S;
                         (Msg, S) -> Parent ! Msg, S
                      end,
                      dummy_state}),

    ok.

check_for_overflow(State = #state{paused=false, overflow=true,
                                  trace_spec=TraceSpec}) ->
    {_, QLen} = erlang:process_info(self(), message_queue_len),
    case QLen =< 100 of
        true ->
            set_trace_opts(true, TraceSpec, State#state.node),
            State#state{overflow=false};
        false ->
            State
    end;
check_for_overflow(State = #state{paused=false, overflow=false,
                                  trace_spec=TraceSpec}) ->
    {_, QLen} = erlang:process_info(self(), message_queue_len),
    case QLen >= 1000 of
        true ->
            set_trace_opts(false, TraceSpec, State#state.node),
            State#state{overflow=true};
        false ->
            State
    end;
check_for_overflow(State) ->
    State.

setup_trace(pause, State) ->
    set_trace_opts(false, State#state.trace_spec, State#state.node),
    State#state{paused = true};
setup_trace(resume, State = #state{trace_spec=Spec}) ->
    setup_trace(Spec, State#state{trace_spec=undefined});
setup_trace(Spec, State = #state{trace_spec=undefined}) ->
    set_trace_opts(true, Spec, State#state.node),
    State#state{trace_spec=Spec, paused=false};
setup_trace(Spec, State) ->
    catch set_trace_opts(false, State#state.trace_spec, State#state.node),
    setup_trace(Spec, State#state{trace_spec=undefined}).

set_trace_opts(How, {spawner, SpwPid, _Sampl}, Node) ->
    trace(Node, SpwPid, How, [procs, timestamp]);
set_trace_opts(How, all, Node) ->
    trace(Node, all, How, [call, timestamp]);
set_trace_opts(How, Pid, Node) when is_pid(Pid) ->
    trace(Node, Pid, How, [call, timestamp]);
set_trace_opts(_How, undefined, _Node) ->
    true.

%% Maybe execute on remote node

trace_pattern(Node, MFA, MatchSpec, Flags) when Node =:= node() ->
    erlang:trace_pattern(MFA, MatchSpec, Flags);
trace_pattern(Node, MFA, MatchSpec, Flags) ->
    rpc:call(Node, erlang, trace_pattern, [MFA, MatchSpec, Flags]).

trace(Node, PidSpec, How, Flags) when Node =:= node() ->
    erlang:trace(PidSpec, How, Flags);
trace(Node, PidSpec, How, Flags) ->
    rpc:call(Node, erlang, trace, [PidSpec, How, Flags]).

netload(Node, Mod) ->
  {Mod, Bin, Fname} = code:get_object_code(Mod),
  {module, Mod} = rpc:call(Node, code, load_binary, [Mod, Fname, Bin]).
