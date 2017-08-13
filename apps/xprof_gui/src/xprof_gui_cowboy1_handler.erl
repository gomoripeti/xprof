-module(xprof_gui_cowboy1_handler).

-ifdef(COWBOY_1).
%% Cowboy 1.0 support

-behavior(cowboy_http_handler).

-define(HDR_JSON, [{<<"content-type">>, <<"application/json">>}]).
-define(HDR_NO_CONTENT, [{<<"content-type">>, <<"application/octet-stream">>}]).

%% Cowboy 1.0 callbacks

-export([init/3,
         handle/2,
         terminate/3
        ]).

init(_Type, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req0, State) ->
    {What, _} = cowboy_req:binding(what, Req0),
    {Params, _} = cowboy_req:qs_val(Req0),
    {ok, Req} = handle_req(What, Params, Req0),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

-else.
%% Cowboy 2.0 support

-behavior(cowboy_handler).

-define(HDR_JSON, #{<<"content-type">> => <<"application/json">>}).

%% In case an XHR receives no content with no content-type Firefox will emit
%% the following error: "XML Parsing Error: no root element found..."
%% As a workaround always return a content-type of octet-stream with
%% 204 No Content responses
-define(HDR_NO_CONTENT, #{<<"content-type">> => <<"application/octet-stream">>}).

%% Cowboy 2.0 callbacks

-export([init/2]).

init(Req0, State) ->
    What = cowboy_req:binding(what, Req0),
    Params = cowboy_req:parse_qs(Req0),
    Req = handle_req(What, Params, Req0),
    {ok, Req, State}.

-endif.

%% Private

%% Common part to handle different HTTP requests

handle_req(<<"funs">>, Params, Req) ->
    Query = get_query(Params),

    Funs = xprof_core:get_matching_mfas_pp(Query),
    Json = jsone:encode(Funs),

    lager:debug("Returning ~b functions matching phrase \"~s\"", [length(Funs), Query]),

    cowboy_req:reply(200, ?HDR_JSON, Json, Req);
handle_req(<<"mon_start">>, Params, Req) ->
    Query = get_query(Params),

    lager:info("Starting monitoring via web on '~s'~n", [Query]),

    case xprof_core:monitor_pp(Query) of
        ok ->
            cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);
        {error, already_traced} ->
            cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);
        _Error ->
            cowboy_req:reply(400, Req)
    end;

handle_req(<<"mon_stop">>, Params, Req) ->
    MFA = {M, F, A} = get_mfa(Params),

    lager:info("Stopping monitoring via web on ~w:~w/~w~n",[M, F, A]),

    xprof_core:demonitor(MFA),
    cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);

handle_req(<<"mon_get_all">>, _Params, Req) ->
    Funs = xprof_core:get_all_monitored(),
    FunsArr = [[Mod, Fun, Arity, Query]
               || {{Mod, Fun, Arity}, Query} <- Funs],
    Json = jsone:encode(FunsArr),
    cowboy_req:reply(200, ?HDR_JSON, Json, Req);

handle_req(<<"data">>, Params, Req) ->
    MFA = get_mfa(Params),
    LastTS = get_int(<<"last_ts">>, Params, 0),

    case xprof_core:get_data(MFA, LastTS) of
        {error, not_found} ->
            cowboy_req:reply(404, Req);
        Vals ->
            Json = jsone:encode([{Val} || Val <- Vals]),
            cowboy_req:reply(200, ?HDR_JSON, Json, Req)
    end;

handle_req(<<"trace_set">>, Params, Req) ->
    case proplists:get_value(<<"spec">>, Params) of
        <<"all">> ->
            xprof_core:trace(all),
            cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);
        <<"pause">> ->
            xprof_core:trace(pause),
            cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);
        Spec ->
            lager:info("Wrong spec for tracing: ~p",[Spec]),
            cowboy_req:reply(400, Req)
    end;

handle_req(<<"trace_status">>, _Params, Req) ->
    {_, Status} = xprof_core:get_trace_status(),
    Json = jsone:encode({[{status, Status}]}),
    cowboy_req:reply(200, ?HDR_JSON, Json, Req);

handle_req(<<"capture">>, Params, Req) ->
    MFA = {M, F, A} = get_mfa(Params),
    Threshold = get_int(<<"threshold">>, Params),
    Limit = get_int(<<"limit">>, Params),

    lager:info("Capture ~b calls to ~w:~w/~w~n exceeding ~b ms",
               [Limit, M, F, A, Threshold]),

    {ok, CaptureId} = xprof_core:capture(MFA, Threshold, Limit),
    Json = jsone:encode({[{capture_id, CaptureId}]}),

    cowboy_req:reply(200, ?HDR_JSON, Json, Req);

handle_req(<<"capture_stop">>, Params, Req) ->
    MFA = get_mfa(Params),

    lager:info("Stopping slow calls capturing for ~p", [MFA]),

    case xprof_core:capture_stop(MFA) of
        ok ->
            cowboy_req:reply(204, ?HDR_NO_CONTENT, Req);
        {error, not_found} ->
            cowboy_req:reply(404, Req)
    end;

handle_req(<<"capture_data">>, Params, Req) ->
    MFA  = get_mfa(Params),
    Offset = get_int(<<"offset">>, Params, 0),

    case xprof_core:get_captured_data_pp(MFA, Offset) of
        {error, not_found} ->
            cowboy_req:reply(404, Req);
        {ok, {Id, Threshold, Limit, HasMore}, Items} ->
            Json = jsone:encode({[{capture_id, Id},
                                  {threshold, Threshold},
                                  {limit, Limit},
                                  {items, Items},
                                  {has_more, HasMore}]}),
            cowboy_req:reply(200, ?HDR_JSON, Json, Req)
    end;

handle_req(<<"mode">>, _Params, Req) ->
    Mode = xprof_core:get_mode(),
    Json = jsone:encode({[{mode, Mode}]}),
    cowboy_req:reply(200, ?HDR_JSON, Json, Req).

%% Helpers

-spec get_mfa([{binary(), binary() | true}]) -> xprof_core:mfa_id().
get_mfa(Params) ->
    {binary_to_atom(proplists:get_value(<<"mod">>, Params), latin1),
     binary_to_atom(proplists:get_value(<<"fun">>, Params), latin1),
     case proplists:get_value(<<"arity">>, Params) of
         <<"_">> -> '_';
         Arity -> binary_to_integer(Arity)
     end}.

-spec get_query([{binary(), binary() | true}]) -> binary().
get_query(Params) ->
    proplists:get_value(<<"query">>, Params, <<"">>).

-spec get_int(binary(), [{binary(), binary() | true}]) -> integer().
get_int(Key, Params) ->
    {_, BinValue} = lists:keyfind(Key, 1, Params),
    binary_to_integer(BinValue).

-spec get_int(binary(), [{binary(), binary() | true}], integer()) -> integer().
get_int(Key, Params, Default) ->
    case lists:keyfind(Key, 1, Params) of
        {_, BinValue} ->
            binary_to_integer(BinValue);
        _ ->
            Default
    end.
