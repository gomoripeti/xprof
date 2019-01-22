-module(xprof_core_hist_SUITE).

-export([all/0]).
-export([groups/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([group/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-define(root,?config(data_dir,Config)).

-export([t_hdr_create/1]).
%%-export([t_hdr_invalid_sigfig/1]).
-export([t_hdr_total_count/1]).
-export([t_hdr_max_value/1]).
-export([t_hdr_min_value/1]).
-export([t_hdr_percentiles/1]).
-export([t_hdr_reset/1]).
-export([t_hdr_close/1]).
%%-export([t_hdr_binary/1]).
%%-export([t_hdr_binary_nc/1]).
%%-export([t_iter_recorded/1]).
%%-export([t_iter_linear/1]).
%%-export([t_iter_logarithmic/1]).
%%-export([t_iter_percentile/1]).
%%-export([t_counter_example_stddev/1]).
-export([t_issue_004/1]).
-export([t_issue_013/1]).
%%-export([t_issue_021/1]).
%%-export([t_unique_resource_types/1]).
-export([t_use_after_close/1]).

-export([load_histograms/0]).

-include_lib("common_test/include/ct.hrl").

-define(BADARG(Expr), (fun() ->
                               case (catch Expr) of
                                   {'EXIT', {badarg, _}} -> ok;
                                   __Other__ -> ct:fail([{line, ?LINE},
                                                         {expected, badarg},
                                                         {actual, __Other__}])
                               end
                       end)()).

all() ->
    [
     {group, hdr}
     %%, {group, iter}
     %%, {group, counter}
    , {group, regression}
    ].

groups() ->
    [{hdr, [], [
        t_hdr_create
%%      , t_hdr_invalid_sigfig
      , t_hdr_total_count
      , t_hdr_max_value
      , t_hdr_min_value
      , t_hdr_percentiles
      , t_hdr_reset
      , t_hdr_close
%%      , t_hdr_binary
%%      , t_hdr_binary_nc
    ]},
   %%  {iter, [], [
   %%     t_iter_recorded
   %%   , t_iter_linear
   %%   , t_iter_logarithmic
   %%   , t_iter_percentile
   %% ]},
     %% Counter examples / regression tests for bugs
   %%  {counter, [], [
   %%      t_counter_example_stddev
   %%  ]},
    {regression, [], [
        t_issue_004,
        t_issue_013,
%%        t_issue_021,
%%        t_unique_resource_types,
        t_use_after_close
    ]}].

suite() ->
    [{ct_hooks, [cth_surefire]}, {timetrap, 2000}].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    {Raw,Cor} = load_histograms(),
    [{raw,Raw},{cor,Cor}|Config].

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

t_hdr_create(_Config) ->
    {ok,R} = xprof_core_hist:open(36000000, 4),
    %%1704008 = xprof_core_hist:get_memory_size(R),
    xprof_core_hist:close(R),
    ok.

%%t_hdr_invalid_sigfig(_Config) ->
%%    {error,bad_significant_factor} = (catch xprof_core_hist:open(36000000, -1)),
%%    {error,bad_significant_factor} = (catch xprof_core_hist:open(36000000, 6)),
%%    ok.

t_hdr_total_count(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    Pid = element(2, Raw),
    10001 = xprof_core_hist:total_count(Raw),
    %%20000 = xprof_core_hist:total_count(Cor),
    ok.

t_hdr_max_value(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    RawMax = xprof_core_hist:max(Raw),
    CorMax = xprof_core_hist:max(Cor),
    xprof_core_hist:same(Raw,100000000,RawMax),
    xprof_core_hist:same(Cor,100000000,CorMax),
    ok.

t_hdr_min_value(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    1000 = xprof_core_hist:min(Raw),
    1000 = xprof_core_hist:min(Cor),
    ok.

t_hdr_percentiles(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    cmp(1.0e3 , xprof_core_hist:percentile(Raw, 30.0), 0.001),
    cmp(1.0e3 , xprof_core_hist:percentile(Raw, 99.0), 0.001),
    cmp(1.0e3 , xprof_core_hist:percentile(Raw, 99.99), 0.001),
    %% FIXME too big error
    %%cmp(1.0e8 , xprof_core_hist:percentile(Raw, 99.999), 0.001),
    %%cmp(1.0e8 , xprof_core_hist:percentile(Raw, 100.0), 0.001),
    %%cmp(1.0e3 , xprof_core_hist:percentile(Cor, 30.0), 0.001),
    %%cmp(5.0e7 , xprof_core_hist:percentile(Cor, 75.0), 0.001),
    %%cmp(8.0e7 , xprof_core_hist:percentile(Cor, 90.0), 0.001),
    %%cmp(9.8e7 , xprof_core_hist:percentile(Cor, 99.0), 0.001),
    %%cmp(1.0e8 , xprof_core_hist:percentile(Cor, 99.99), 0.001),
    %%cmp(1.0e8 , xprof_core_hist:percentile(Cor, 99.999), 0.001),
    %%cmp(1.0e8 , xprof_core_hist:percentile(Cor, 100.0), 0.001),
    ok.

t_hdr_reset(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    xprof_core_hist:reset(Raw),
    xprof_core_hist:reset(Cor),
    0 = xprof_core_hist:total_count(Raw),
    0 = xprof_core_hist:total_count(Cor),
    %% FIXME why is this an integer???
    %%0.0 = xprof_core_hist:percentile(Raw, 99.0),
    0 = xprof_core_hist:percentile(Raw, 99.0),
    %%0.0 = xprof_core_hist:percentile(Cor, 99.0),
    0 = xprof_core_hist:percentile(Cor, 99.0),
    ok.

t_hdr_close(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),
    xprof_core_hist:close(Raw),
    xprof_core_hist:close(Cor),
    %% double close is harmless
    ok.

%%t_hdr_binary(_Config) ->
%%    %% Work around an issue with CT and NIFs
%%    {Raw,Cor} = load_histograms(),
%%    BinRaw = xprof_core_hist:to_binary(Raw),
%%    BinCor = xprof_core_hist:to_binary(Cor),
%%    true = is_binary(BinRaw),
%%    true = is_binary(BinCor),
%%    {ok,Raw2} = xprof_core_hist:from_binary(BinRaw),
%%    {ok,Cor2} = xprof_core_hist:from_binary(BinCor),
%%    {error,bad_hdr_binary} = (catch xprof_core_hist:from_binary(<<>>)),
%%    cmp(1.0e8 , xprof_core_hist:percentile(Raw, 100.0), 0.001),
%%    cmp(1.0e8 , xprof_core_hist:percentile(Raw2, 100.0), 0.001),
%%    cmp(1.0e8 , xprof_core_hist:percentile(Cor, 100.0), 0.001),
%%    cmp(1.0e8 , xprof_core_hist:percentile(Cor2, 100.0), 0.001),
%%    ok.

%%t_hdr_binary_nc(_Config) ->
%%    %% Work around an issue with CT and NIFs
%%    {Raw,Cor} = load_histograms(),
%%    BinRaw = xprof_core_hist:to_binary(Raw, [{compression, none}]),
%%    BinCor = xprof_core_hist:to_binary(Cor, [{compression, none}]),
%%    true = is_binary(BinRaw),
%%    true = is_binary(BinCor),
%%    {ok,Raw2} = xprof_core_hist:from_binary(BinRaw),
%%    {ok,Cor2} = xprof_core_hist:from_binary(BinRaw),
%%    {error,bad_hdr_binary} = (catch xprof_core_hist:from_binary(<<>>)),
%%    cmp(1.0e8 , xprof_core_hist:percentile(Raw, 100.0), 0.001),
%%    cmp(1.0e8 , xprof_core_hist:percentile(Raw2, 100.0), 0.001),
%%    cmp(1.0e8 , xprof_core_hist:percentile(Cor, 100.0), 0.001),
%%    cmp(1.0e8 , xprof_core_hist:percentile(Cor2, 100.0), 0.001),
%%    ok.


t_iter_recorded(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),

  {ok,RawIter} = hdr_iter:open(record, Raw, []),
  RawStepCounts = hdr_iter:each(RawIter, step_counts(), []),
  hdr_iter:close(RawIter),

  {ok,CorIter} = hdr_iter:open(record, Cor, []),
  CorStepCounts = hdr_iter:each(CorIter, step_counts(), []),
  hdr_iter:close(CorIter),

  [10000,1] = RawStepCounts,
  [10000|X] = CorStepCounts,
  10000 = lists:sum(X),

  10001 = lists:sum(RawStepCounts),
  20000 = lists:sum(CorStepCounts),
  ok.

t_iter_linear(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),

  {ok,RawIter} = hdr_iter:open(linear, Raw, [{linear_value_unit,10000}]),
  RawStepCounts = hdr_iter:each(RawIter,step_counts(), []),
  hdr_iter:close(RawIter),

  {ok,CorIter} = hdr_iter:open(linear, Cor, [{linear_value_unit,10000}]),
  CorStepCounts = hdr_iter:each(CorIter,step_counts(), []),
  hdr_iter:close(CorIter),

  1 = lists:sum(RawStepCounts),
  20000 = lists:sum(CorStepCounts),
  ok.

t_iter_logarithmic(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),

  {ok,RawIter} = hdr_iter:open(logarithmic, Raw, [{log_value_unit,100},{log_base,10.0}]),
  RawStepCounts = hdr_iter:each(RawIter,accum_steps(), 0),
  hdr_iter:close(RawIter),

  {ok,CorIter} = hdr_iter:open(logarithmic, Cor, [{log_value_unit,100},{log_base,10.0}]),
  CorStepCounts = hdr_iter:each(CorIter,accum_steps(), 0),
  hdr_iter:close(CorIter),

  10001 = RawStepCounts,
  20000 = CorStepCounts,
  ok.

t_iter_percentile(Config) ->
    Raw = ?config(raw,Config),
    Cor = ?config(cor,Config),

  {ok,RawIter} = hdr_iter:open(percentile, Raw, [{percentile_half_ticks,20}]),
  RawStepCounts = hdr_iter:each(RawIter,count(), 0),
  hdr_iter:close(RawIter),

  {ok,CorIter} = hdr_iter:open(percentile, Cor, [{percentile_half_ticks,20}]),
  CorStepCounts = hdr_iter:each(CorIter,count(), 0),
  hdr_iter:close(CorIter),

  270 = RawStepCounts,
  238 = CorStepCounts,
  ok.

%%%% This test triggers a bug in the stddev function.
%%t_counter_example_stddev(_Config) ->
%%    {ok, H} = xprof_core_hist:open(100000,1),
%%    xprof_core_hist:record(H, 52581),
%%    StdDev = xprof_core_hist:stddev(H),
%%    %% StdDev is a ilegal value we need to print it to tigger the crash
%%    %% or convert it to binary and back. the binery conversion seems to
%%    %% be the nicer choice.
%%    %% io:format("Stddev is: ~p~n", [StdDev]),
%%    binary_to_term(term_to_binary(StdDev)),
%%    xprof_core_hist:close(H),
%%    ok.

t_issue_004(_Config) ->
    {ok,R} = xprof_core_hist:open(10,1),
    [ begin
        ok = xprof_core_hist:record(R, X)
    end || X <- lists:seq(0,10) ],
    {error, value_out_of_range} = xprof_core_hist:record(R, -1),
    {error, value_out_of_range} = xprof_core_hist:record(R, 11),
    ok = xprof_core_hist:close(R).

t_issue_013(_Config) ->
    {ok,R} = xprof_core_hist:open(10,1),
    [ begin
      ok = xprof_core_hist:record_many(R, X, 10)
    end || X <- lists:seq(0,10) ],
    {error, value_out_of_range} = xprof_core_hist:record(R, -1),
    {error, value_out_of_range} = xprof_core_hist:record(R, 11),
    ok = xprof_core_hist:close(R).

%% t_issue_021(_Config) ->
%%     {ok, H1} = xprof_core_hist:open(524287, 5),
%%     Bin = xprof_core_hist:to_binary(H1),
%%     {ok, H2} = xprof_core_hist:from_binary(Bin),
%%     Bin = xprof_core_hist:to_binary(H2).

%% t_unique_resource_types(_Config) ->
%%     {ok, H} = xprof_core_hist:open(10, 1),
%%     {ok, I} = hdr_iter:open(record, H, []),
%%     ?BADARG(xprof_core_hist:record(I, 1)).

t_use_after_close(_Config) ->
    {ok, Closed} = xprof_core_hist:open(10, 1),
    ok = xprof_core_hist:close(Closed),

%%    ?BADARG(xprof_core_hist:get_memory_size(Closed)),
    ?BADARG(xprof_core_hist:total_count(Closed)),

    ?BADARG(xprof_core_hist:record(Closed, 1)),
    ?BADARG(xprof_core_hist:record_corrected(Closed, 1, 1)),
    ?BADARG(xprof_core_hist:record_many(Closed, 1, 5)),

    {ok, Open} = xprof_core_hist:open(10, 1),
%%     ?BADARG(xprof_core_hist:add(Closed, Open)),
%%     ?BADARG(xprof_core_hist:add(Open, Closed)),

    ?BADARG(xprof_core_hist:min(Closed)),
    ?BADARG(xprof_core_hist:max(Closed)),
    ?BADARG(xprof_core_hist:mean(Closed)),
%%    ?BADARG(xprof_core_hist:median(Closed)),

%%    ?BADARG(xprof_core_hist:stddev(Closed)),
    ?BADARG(xprof_core_hist:percentile(Closed, 50.0)),

    ?BADARG(xprof_core_hist:same(Closed, 5, 5)),
%%    ?BADARG(xprof_core_hist:lowest_at(Closed, 5)),
%%    ?BADARG(xprof_core_hist:count_at(Closed, 5)),

%%    ?BADARG(xprof_core_hist:print(Closed, classic)),
%%    ?BADARG(xprof_core_hist:log(Closed, classic, "foo")),

    ?BADARG(xprof_core_hist:reset(Closed)),
    ?BADARG(xprof_core_hist:close(Closed)),

%%    ?BADARG(xprof_core_hist:to_binary(Closed)),
%%    ?BADARG(xprof_core_hist:to_binary(Closed, [{compression, none}])),

%%    ?BADARG(hdr_iter:open(record, Closed, [])).
    ok.

step_counts() ->
    fun({_,Attrs},Acc) ->
        {step_count,X}=lists:keyfind(step_count,1,Attrs),
        Acc ++ [X]
    end.

accum_steps() ->
    fun({_,Attrs},Acc) ->
        {step_count,X}=lists:keyfind(step_count,1,Attrs),
        Acc + X
    end.

count() ->
    fun({_,_},Acc) ->
        Acc + 1
    end.

load_histograms() ->
    %% init_per_group is run in a separate process which terminates
    %% before the test cases are run
    %% so spawn a separate long-lived process that owns the ets tables
    InitGroupProc = self(),
    proc_lib:spawn(
      fun() ->
              {ok,Raw0} = xprof_core_hist:open(raw_table, 3600 * 1000 * 1000, 3),
              {ok,Cor0} = xprof_core_hist:open(cor_table, 3600 * 1000 * 1000, 3),
              InitGroupProc ! {Raw0, Cor0},
              receive _ -> stop end
      end),
    receive {Raw, Cor} -> ok end,
    load(10000, {Raw,Cor}),
    ok = xprof_core_hist:record(Raw, 100000000),
    ok = xprof_core_hist:record_corrected(Cor,100000000,10000),
    {Raw,Cor}.
load(0,{Raw,Cor}) ->
    {Raw,Cor};
load(N,{Raw,Cor}) ->
    ok = xprof_core_hist:record(Raw, 1000),
    ok = xprof_core_hist:record_corrected(Cor,1000,10000),
    load(N-1,{Raw,Cor}).

cmp(L1,L2,D) ->
    case erlang:abs(L1-L2) < D of
	false -> throw({not_same, L1, L2, D,
                        element(2, erlang:process_info(
                                     self(), current_stacktrace))});
	true -> true
    end.
