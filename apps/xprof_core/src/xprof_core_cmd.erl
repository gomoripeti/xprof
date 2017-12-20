%%% There are 3 types of cmds
%%% - tracing functions -> mfaspec present -> could use meta-tracing with trace_pattern
%%%   -> can be turned off by trace_pattern
%%%   - cmd id = mfaid
%%% - tracing send or receive -> no mfa, no meta-tracing, but can use trace_pattern (to match on message/sender/receiver)
%%%   -> can be turned off by trace_pattern
%%%   - cmd id = ??? (single tracer per cmd-name or trace tag)
%%% - other -> no trace_pattern -> xprof_core_tracer should find out where to send trace based on trace tag (maybe)
%%%   -> can only be turned off by trace/3
%%%   - cmd id = ??? (single tracer per cmd-name or trace tag)
-module(xprof_core_cmd).

-export([process_query/2,
         process_cmd/2]).

%%
%% Callback functions that need to be implemented
%% for a command behvaiour
%%

%% Return list of mandatory params
-callback mandatory_params() -> [atom()].

%% Convert param value from syntax-tree format to Erlang term or expression
%% (Useful to implement some syntactic sugar/shorthands)
-callback param_from_ast(Key :: atom(), Ast :: erl_parse:syntax_tree()) ->
    {ok, Value :: term()} | {error, Reason :: term()}.

%% Validate param value and optionally convert to internal format
%% (Called for both params parsed from query string and provided directly as
%% additional params)
-callback param_to_internal(Key :: atom(), Value :: term()) ->
    {ok, NewValue :: term()} | {error, Reason :: term()}.

process_query(Query, AdditionalParams) ->
    case xprof_core_query:parse_query(Query) of
        {error, _} = Error ->
            Error;
        {ok, Cmd, ParamsAst} ->
            %% * lookup cmd callback
            CmdCB = get_cmd_callback(Cmd),

            %% * convert params from AST to term
            %% (could error on unknown params)
            {ok, QueryParams} = params_from_ast(Cmd, ParamsAst, CmdCB, []),

            %% * merge additional params
            %% - if additional params have precedence then it is possible to
            %%   modify the query from gui menu/buttons
            %% - if query params have precedence then we make sure what is
            %%   displayed in query string takes effect
            Params = merge_params(QueryParams, AdditionalParams),

            %% * check all mandatory params are present
            MandatoryParams = CmdCB:mandatory_params(),
            ok = check_mandatory_params(Params, MandatoryParams),

            %% * check param value types as much as possible
            %%   and maybe convert to internal format (eg mfaspec)
            {ok, Options} = params_to_internal(Cmd, Params, CmdCB, []),

            {start_cmd, Cmd, Options, CmdCB, Query}
    end.

process_cmd(Cmd, Params) ->
    %% * lookup cmd callback
    CmdCB = get_cmd_callback(Cmd),

    %% * FIXME figure out some string represention to fake querystring
    %% (probably move to xprof_core_query:fmt_query(Cmd, Params)
    Query =
        case proplists:get_value(mfa, Params) of
            undefined ->
                <<"">>;
            MFAStr when is_list(MFAStr) ->
                list_to_binary(MFAStr);
            {Mod, Fun, Arity} ->
                ModeCb = xprof_core_lib:get_mode_cb(),
                _FormattedMFA = ModeCb:fmt_mfa(Mod, Fun, Arity)
        end,

    %% * check all mandatory params are present
    MandatoryParams = CmdCB:mandatory_params(),
    ok = check_mandatory_params(Params, MandatoryParams),

    %% * check param value types as much as possible
    %%   and maybe convert to internal format (eg mfaspec)
    {ok, Options} = params_to_internal(Cmd, Params, CmdCB, []),

    {start_cmd, Cmd, Options, CmdCB, Query}.

get_cmd_callback(funlatency) ->
    xprof_core_cmd_funlatency;
get_cmd_callback(Cmd) ->
    {error, {unknown_command, Cmd}}.

params_from_ast(Cmd, [{Key, Ast}|ParamsAst], CmdCB, Acc) ->
    case CmdCB:param_from_ast(Key, Ast) of
        {error, Reason} ->
            {error, {convert_param, Cmd, Key, Reason}};
        {ok, Value} ->
            params_from_ast(Cmd, ParamsAst, CmdCB, [{Key, Value}|Acc])
    end;
params_from_ast(_, [], _, Acc) ->
    {ok, lists:reverse(Acc)}.

params_to_internal(Cmd, [{Key, Value}|Params], CmdCB, Acc) ->
    case CmdCB:param_to_internal(Key, Value) of
        {error, Reason} ->
            {error, {check_param, Cmd, Key, Reason}};
        {ok, InternalValue} ->
            params_to_internal(Cmd, Params, CmdCB, [{Key, InternalValue}|Acc])
    end;
params_to_internal(_, [], _, Acc) ->
    {ok, lists:reverse(Acc)}.

merge_params(P1, P2) ->
    lists:foldl(
      fun({Key, _} = Param, P) ->
              lists:keystore(Key, 1, P, Param)
      end, P2, P1).

check_mandatory_params(Params, MandatoryParams) ->
    [case lists:keymember(Key, 1, Params) of
         true -> ok;
         false -> xprof_core_lib:err("Mandatory param ~p missing", [Key])
     end
     || Key <- MandatoryParams],
    ok.
