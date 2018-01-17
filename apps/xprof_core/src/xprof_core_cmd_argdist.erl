-module(xprof_core_cmd_argdist).

-behaviour(xprof_core_cmd).

-export([mandatory_params/0,
         optional_params/0,
         param_from_ast/2,
         param_to_internal/2,

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

get_cmd_id(Params) ->
    MFASpec = proplists:get_value(mfa, Params),
    MFAId = xprof_core_lib:mfaspec2id(MFASpec),
    MFAId.

%% tracer

-record(state, {freq_count = dict:new(),
                total_freq_count = dict:new(),
                max_enum}).

init(Options, _MFASpec) ->
    MaxEnum = proplists:get_value(enum, Options, 10),
    {ok, #state{max_enum = MaxEnum}}.

handle_event({trace_ts, _Pid, call, _MFA, Args, _StartSDL_CreateWindowTime}, _,
             State = #state{freq_count = FreqCount,
                            total_freq_count = TotalFreqCount,
                            max_enum = MaxEnum}) ->
    Key =
        case dict:size(TotalFreqCount) < MaxEnum orelse
             dict:is_key(Args, TotalFreqCount) of
            true -> Args;
            _ -> 'other'
        end,
    {ok, State#state{
           freq_count = dict:update_counter(Key, 1, FreqCount),
           total_freq_count = dict:update_counter(Key, 1, TotalFreqCount)}};
handle_event(_, _, _) ->
    ok.

take_snapshot(State = #state{freq_count = FreqCount,
                             total_freq_count = TotalFreqCount}) ->
    TotalList = lists:keysort(2, dict:to_list(TotalFreqCount)),
    IntervalList = [case dict:find(Key, FreqCount) of
                        {ok, Count} -> {Key, Count};
                        error -> {Key, 0}
                    end
                    || {Key, _TotalCount} <- TotalList],
    {IntervalList, State#state{freq_count = dict:new()}}.
