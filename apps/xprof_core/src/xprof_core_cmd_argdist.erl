-module(xprof_core_cmd_argdist).

-behaviour(xprof_core_cmd).

-export([mandatory_params/0,
         optional_params/0,
         param_from_ast/2,
         param_to_internal/2,
         format_error/1,

         get_cmd_id/1,

         %% tracer callbacks
         init/2,
         handle_event/3,
         take_snapshot/1
        ]).

mandatory_params() ->
    [mfa].

optional_params() ->
    [enum].

param_from_ast(mfa, MfaStr) when is_list(MfaStr) ->
    {ok, MfaStr};
param_from_ast(mfa, _WrongValue) ->
    {error, wrong_value};
param_from_ast(enum, MaxEnumAst) ->
    xprof_core_query:param_to_term(MaxEnumAst);
param_from_ast(_, _) ->
    {error, unknown_param}.

param_to_internal(mfa, Value) ->
    case xprof_core_ms:fun2ms(Value) of
        {ok, {MFAId, {_MSOff, MSOn}}} ->
            {ok, {MFAId, {MSOn, MSOn}}};
        Error ->
            Error
    end;
param_to_internal(enum, MaxEnum) ->
    if is_integer(MaxEnum) andalso MaxEnum >= 2 ->
            {ok, MaxEnum};
       true ->
            {error, wrong_value}
    end;
param_to_internal(_, _) ->
   {error, unknown_param}.

format_error(Str) when is_list(Str) ->
    %% already formatted error from `fun2ms'
    Str.

get_cmd_id(Options) ->
    MFASpec = proplists:get_value(mfa, Options),
    MFAId = xprof_core_lib:mfaspec2id(MFASpec),
    MFAId.

%% tracer

init(Options, _MFASpec) ->
    MaxEnum = proplists:get_value(enum, Options, 10),
    init_state(MaxEnum).

handle_event({trace_ts, _Pid, call, _MFA, Args, _StartTime}, _, State) ->
    update_count(Args, State);
handle_event(_, _, _) ->
    ok.

-ifdef(no_atomics).
%% Old implementation with counters in a dictionary

-record(state, {freq_count = dict:new() :: dict:dict(term(), pos_integer()),
                all_keys = [] :: list(), %% keys in the order of arrival
                max_enum}).

-spec init_state(pos_integer()) -> #state{}.
init_state(MaxEnum) ->
    {ok, #state{max_enum = MaxEnum}}.

-spec update_count(list(), #state{}) -> {ok, #state{}}.
update_count(Args, State = #state{freq_count = FreqCount,
                                  all_keys = AllKeys,
                                  max_enum = MaxEnum}) ->
    {Key, NewAllKeys} =
        case lists:member(Args, AllKeys) of
            true ->
                {Args, AllKeys};
            _ ->
                case AllKeys of
                    ['other'|_] ->
                        {'other', AllKeys};
                    _ ->
                        case length(AllKeys) < MaxEnum of
                            true ->
                                {Args, [Args|AllKeys]};
                            _ ->
                                {'other', ['other'|AllKeys]}
                        end
                end
        end,
    {ok, State#state{
           freq_count = dict:update_counter(Key, 1, FreqCount),
           all_keys = NewAllKeys}}.

take_snapshot(State = #state{freq_count = FreqCount,
                             all_keys = AllKeys}) ->
    IntervalList = [case dict:find(Key, FreqCount) of
                        {ok, Count} -> {Key, Count};
                        error -> {Key, 0}
                    end
                    || Key <- lists:reverse(AllKeys)],
    {IntervalList, State#state{freq_count = dict:new()}}.

-else.
%% New implementation with atomic counters

-record(state, {freq_count :: atomics:atomics_ref(),
                key_to_idx = #{} :: #{term() => pos_integer()}, %% #{key => counter_idx}
                all_keys = [] :: list(), %% keys in the order of arrival
                max_idx :: pos_integer()}).

-spec init_state(pos_integer()) -> #state{}.
init_state(MaxEnum) ->
    %% 1 extra slot for current max idx, and another for the `other' bucket
    MaxIdx = MaxEnum + 2,
    Counters = atomics:new(MaxIdx, []),
    atomics:put(Counters, 1, 1), %% current max idx
    {ok, #state{freq_count = Counters,
                max_idx = MaxIdx}}.

-spec update_count(list(), #state{}) -> {ok, #state{}}.
update_count(Args, State = #state{freq_count = Counters,
                                  key_to_idx = KeyToIdx,
                                  all_keys = AllKeys,
                                  max_idx = MaxIdx}) ->
    case maps:find(Args, KeyToIdx) of
        {ok, Idx} ->
            atomics:add(Counters, Idx, 1),
            {ok, State};
        error ->
            case AllKeys of
                ['other'|_] ->
                    atomics:add(Counters, MaxIdx, 1),
                    {ok, State};
                _ ->
                    case atomics:add_get(Counters, 1, 1) of
                        MaxIdx ->
                            %% reached max, have to add the `other' bucket
                            atomics:add(Counters, MaxIdx, 1),
                            {ok, State#state{all_keys = ['other'|AllKeys]}};
                        NewIdx ->
                            atomics:add(Counters, NewIdx, 1),
                            {ok, State#state{key_to_idx = KeyToIdx#{Args => NewIdx},
                                             all_keys = [Args|AllKeys]}}
                    end
            end
    end.

take_snapshot(#state{freq_count = Counters,
                     all_keys = AllKeys}) ->
    CurrentMaxIdx = atomics:get(Counters, 1),
    snapshot_loop(AllKeys, CurrentMaxIdx, Counters, []).

snapshot_loop([], Idx, _, Acc) ->
    1 = Idx, %% assert
    Acc;
snapshot_loop([Key|AllKeys], Idx, Counters, Acc) ->
    snapshot_loop(AllKeys, Idx - 1, Counters, [{Key, atomics:exchange(Counters, Idx, 0)}|Acc]).

-endif.
