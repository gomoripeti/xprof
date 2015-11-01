%% @doc xprof public API
%% @end
-module(xprof_app).

-behaviour(application).

-include("xprof.hrl").

%% Application callbacks
-export([start/2,stop/1]).

%% API

start(_StartType, _StartArgs) ->
    Node = application:get_env(?APP, node, node()),
    start_cowboy(Node),
    xprof_sup:start_link(Node).

stop(_State) ->
    stop_cowboy(),
    ok.

%% Internal functions

start_cowboy(Node) ->
    Port = application:get_env(?APP, port, ?DEF_WEB_IF_PORT),
    Dispatch = cowboy_router:compile(cowboy_routes(Node)),
    cowboy:start_http(xprof_http_listener, 100, [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]).

cowboy_routes(Node) ->
    [{'_', [{"/api/:what", xprof_web_handler, [{node, Node}]},
            {"/build/[...]", cowboy_static, {priv_dir, ?APP, "build"}},
            {"/", cowboy_static, {priv_file, ?APP, "build/index.html"}}
           ]}].

stop_cowboy() ->
    cowboy:stop_listener(xprof_http_listener).
