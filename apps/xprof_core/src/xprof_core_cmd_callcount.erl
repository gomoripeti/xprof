%% @private
-module(xprof_core_cmd_callcount).

-export([mandatory_params/0,
         optional_params/0,
         param_from_ast/2,
         param_to_internal/2,
         prepare_start/1,
         format_error/1,

         get_cmd_id/1,

         %% tracer callbacks
         no_trace_pattern/0,
         init/2,
         handle_event/3,
         take_snapshot/1
        ]).

mandatory_params() ->
    [mfa].

optional_params() ->
    [].

param_from_ast(mfa, MfaStr) ->
    %% MfaStr is always a string as it is parsed from a query string
    %% just assert it as an internal consistency check
    [_|_] = MfaStr,
    {ok, MfaStr};
param_from_ast(_, _) ->
    {error, unknown_param}.

param_to_internal(mfa, Value) ->
    xprof_core_ms:fun2ms(Value);
param_to_internal(_, _) ->
   {error, unknown_param}.

prepare_start(Options) ->
    MFASpec = proplists:get_value(mfa, Options),
    MFA = xprof_core_lib:mfaspec2id(MFASpec),
    xprof_core_vm_info:ensure_mfa(MFA).

format_error(not_fun) ->
    "Must be a fun of arity 1 or 2";
format_error(wrong_arity) ->
    "Must be a fun of arity 1 or 2";
format_error(Str) when is_list(Str) ->
    %% already formatted error from `fun2ms'
    Str.

get_cmd_id(Options) ->
    MFASpec = proplists:get_value(mfa, Options),
    MFAId = xprof_core_lib:mfaspec2id(MFASpec),
    MFAId.

%% tracer

-record(state, {mfa
               }).

no_trace_pattern() ->
    true.

init(Options, _MFASpec) ->
    MFA = get_cmd_id(Options),
    case erlang:trace_pattern(MFA, true, [call_count, meta]) of
        0 -> io:format("unexpected~n", []),
             stop;
        _ ->
            {ok, #state{mfa = MFA}}
    end.

handle_event(_, _, _State) ->
    %% No events are expected
    ok.

take_snapshot(#state{mfa = MFA}) ->
    Snapshot = get_current_count(MFA),
    %% reset
    erlang:trace_pattern(MFA, restart, [call_count, meta]),
    Snapshot.

%% helpers for tracer callbacks

get_current_count(MFA) ->
    {call_count, Count} = erlang:trace_info(MFA, call_count),
    [{count, Count}].
