%%%
%%% High Dynamic Range (HDR) Histogram for Erlang
%%%
%%% This implementation is based on the Elixir version found at
%%% https://github.com/2nd/histogrex/ with adjustments based on
%%% https://github.com/HdrHistogram/hdr_histogram_erl.
%%%

%%%
%%% The MIT License (MIT)
%%% 
%%% Copyright (c) 2017 Second Spectrum
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.

-module(xprof_core_hist).

-export([new/3,
         new_concurrent/4,
         record/3,
         reset/1,
         delete/1,

         total_count/1,
         max/1,
         min/1,
         mean/1,
         value_at_quantile/2,

         stats/1
        ]).

%% compatibility API with hdr_histogram_erl for testing
-export([open/2,
         open/3,
         close/1,
         record/2,
         record_many/3,
         percentile/2,
         get_total_count/1,
         same/3
        ]).

-define(TABLE, ?MODULE).

-define(TOTAL_COUNT_INDEX, 2).

-record(hist,
        {table,
         %% field names from elixir
         name,
         %% registrar,
         bucket_count,
         counts_length,
         unit_magnitude,
         sub_bucket_mask,
         sub_bucket_count,
         sub_bucket_half_count,
         sub_bucket_half_count_magnitude
         %% template

         %% field names from C
         , min %% lowest_trackable_value,
         , max %% highest_trackable_value,
         %% unit_magnitude,
         , precision %% significant_figures
         %% sub_bucket_half_count_magnitude,
         %% sub_bucket_half_count,
         %% sub_bucket_mask,
         %% sub_bucket_count,
         %% bucket_count,
         %% min_value,
         %% max_value,
         %% normalizing_index_offset,
         %% conversion_ratio,
         %% counts_len,
         %% total_count,
         %% %%counts :: tid()
        }).

%% alias from hdr_histogram NIF API
open(Max, Prec) ->
    new(1, Max, Prec).

open(Name, Max, Prec) ->
    new_concurrent(Name, 1, Max, Prec).

record(H, V) ->
    record(H, V, 1).

record_many(H, V, N) ->
    record(H, V, N).

close(H) ->
    delete(H).

percentile(H, Q) ->
    value_at_quantile(H, Q).

get_total_count(H) ->
    total_count(H).

same(H, A, B) ->
    ets:first(H#hist.table), %% badarg if H was deleted (table does not exist)
    lowest_equivalent_value(H, A) =:= lowest_equivalent_value(H, B).

%%-----------

new(Min, Max, Precision) ->
    Tid = ets:new(?MODULE, [set, private]),
    do_new(Tid, Min, Max, Precision).

new_concurrent(Name, Min, Max, Precision) ->
    Tid = ets:new(Name, [set, public, {write_concurrency, true}]),
    do_new(Tid, Min, Max, Precision).

do_new(Table, Min, Max, Precision)
  when Min > 0 andalso Max > Min
       andalso 1 =< Precision andalso Precision =< 5 ->

    Largest_value_with_single_unit_resolution = 2 * math:pow(10, Precision),
    Sub_bucket_count_magnitude = int_ceil(math:log2(Largest_value_with_single_unit_resolution)),

    Sub_bucket_half_count_magnitude =
        case Sub_bucket_count_magnitude < 1 of
            true -> 1;
            false -> Sub_bucket_count_magnitude - 1
        end,

    Unit_magnitude =
        case int_floor(math:log2(Min)) of
            N when N < 0 -> 0;
            N -> N
        end,

    Sub_bucket_count = round(math:pow(2, Sub_bucket_half_count_magnitude + 1)),
    Sub_bucket_half_count = round(Sub_bucket_count / 2),
    Sub_bucket_mask = (Sub_bucket_count - 1) bsl Unit_magnitude,

    Bucket_count = calculate_bucket_count(Sub_bucket_count bsl Unit_magnitude, Max, 1),
    Counts_length = round((Bucket_count + 1) * (Sub_bucket_count / 2)),

    %%Template = create_row(Name, Counts_length)

    H = #hist{
           table = Table,
           name = hist_key,
           %%template = Template,
           %%registrar = Registrar,
           bucket_count = Bucket_count,
           counts_length = Counts_length,
           unit_magnitude = Unit_magnitude,
           sub_bucket_mask = Sub_bucket_mask,
           sub_bucket_count = Sub_bucket_count,
           sub_bucket_half_count = Sub_bucket_half_count,
           sub_bucket_half_count_magnitude = Sub_bucket_half_count_magnitude,

           min = Min,
           max = Max,
           precision = Precision
          },
    reset(H),
    {ok, H}.

record(H, Value, N) when is_integer(Value), is_integer(N), N > 0 ->
    Index = get_value_index(H, Value),
    case H#hist.max < Value orelse
        Index < 0 orelse H#hist.counts_length =< Index of
        true ->
            {error, value_out_of_range};
        false ->
            ets:update_counter(H#hist.table,
                               H#hist.name,
                               [{?TOTAL_COUNT_INDEX, N},
                                {Index + ?TOTAL_COUNT_INDEX + 1, N}]),
            ok
    end.

reset(H) ->
    ets:insert(H#hist.table, create_row(H#hist.name, H#hist.counts_length)),
    ok.

delete(H) ->
    ets:delete(H#hist.table),
    ok.

%% @doc Get the total number of recorded values. This is O(1)
-spec total_count(#hist{}) -> non_neg_integer().
total_count(H) ->
    Counts = get_counts(H),
    element(?TOTAL_COUNT_INDEX, Counts).

max(H) ->
    do_max(iterator(H)).

min(H) ->
    do_min(iterator(H)).

mean(H) ->
    V = do_mean(iterator(H)),
    %% the NIF does this rounding on the value returned from c code
    round_to_significant_figures(V, H#hist.precision).

-spec value_at_quantile(#hist{}, float()) -> float().
value_at_quantile(H, Q) when Q > 0 andalso Q =< 100 ->
    V = do_value_at_quantile(iterator(H), Q),
    %% the NIF does this rounding on the value returned from c code
    round_to_significant_figures(V, H#hist.precision).

stats(H) ->
    It = iterator(H),
    [{count, do_total_count(It)},
     {min, do_min(It)},
     {mean, do_mean(It)},
     {max, do_max(It)},
     {p50, do_value_at_quantile(It, 50.0)},
     {p75, do_value_at_quantile(It, 75.0)},
     {p90, do_value_at_quantile(It, 90.0)},
     {p99, do_value_at_quantile(It, 99.0)}
    ].

%% Helper functions

round_to_significant_figures(0, _) ->
    0;
round_to_significant_figures(V, Prec) ->
    Factor = math:pow(10.0, Prec - int_ceil(math:log10(abs(V)))),
    round(V * Factor) / Factor.

calculate_bucket_count(Smallest_untrackable_value, Max, Bucket_count) ->
    case Smallest_untrackable_value < Max of
        false -> Bucket_count;
        true -> calculate_bucket_count((Smallest_untrackable_value bsl 1),
                                       Max, Bucket_count + 1)
    end.

get_value_index(H, Value) ->
    {Bucket, Sub} = get_bucket_indexes(H, Value),
    get_count_index(H, Bucket, Sub).

get_bucket_indexes(H, Value) ->
    Ceiling = bit_length((Value bor H#hist.sub_bucket_mask), 0),
    Bucket_index = Ceiling - H#hist.unit_magnitude - (H#hist.sub_bucket_half_count_magnitude + 1),

    Sub_bucket_index = Value bsr (Bucket_index + H#hist.unit_magnitude),
    {Bucket_index, Sub_bucket_index}.

get_bucket_indexes_from_index(H, Index) when Index < H#hist.sub_bucket_half_count ->
    {0, Index};
get_bucket_indexes_from_index(H, Index) ->
    %%Magn = H#hist.sub_bucket_half_count_magnitude,
    Bucket_index = (Index bsr H#hist.sub_bucket_half_count_magnitude) - 1,
    Sub_bucket_index = (Index + H#hist.sub_bucket_half_count)
        - ((Bucket_index + 1) bsl H#hist.sub_bucket_half_count_magnitude),
    {Bucket_index, Sub_bucket_index}.

bit_length(Value, N) when Value >= 32768 ->
    bit_length((Value bsr 16), N + 16);
bit_length(Value, N) ->
    {Value2, N2} = case Value >= 128 of
                       true -> {(Value bsr 8), N + 8};
                       false -> {Value, N}
                   end,

    {Value3, N3} = case Value2 >= 8 of
                       true -> {(Value2 bsr 4), N2 + 4};
                       false -> {Value2, N2}
                   end,

    {Value4, N4} = case Value3 >= 2 of
                       true -> {(Value3 bsr 2), N3 + 2};
                       false -> {Value3, N3}
                   end,

    case Value4 =:= 1 of
        true -> N4 + 1;
        false -> N4
    end.

get_count_index(H, Bucket_index, Sub_bucket_index) ->
    Bucket_base_index =
        (Bucket_index + 1) bsl H#hist.sub_bucket_half_count_magnitude,
    Offset_in_bucket = Sub_bucket_index - H#hist.sub_bucket_half_count,
    Bucket_base_index + Offset_in_bucket.

create_row(Name, Count) ->
    %% +2 for name and total_count that we'll store at the start
    erlang:make_tuple(Count + 2, 0, [{1, Name}]).

get_counts(H) ->
    case ets:lookup(H#hist.table, H#hist.name) of
        [] ->
            throw(data_missing_from_ets);
        [Counts] ->
            Counts
    end.

value_from_index(H, Bucket_index, Sub_bucket_index) ->
    Sub_bucket_index bsl (Bucket_index + H#hist.unit_magnitude).

highest_equivalent_value(H, Bucket_index, Sub_bucket_index) ->
    next_non_equivalent_value(H, Bucket_index, Sub_bucket_index) - 1.

lowest_equivalent_value(H, Value) ->
    {Bucket_index, Sub_bucket_index} = get_bucket_indexes(H, Value),
    lowest_equivalent_value(H, Bucket_index, Sub_bucket_index).

lowest_equivalent_value(H, Bucket_index, Sub_bucket_index) ->
    value_from_index(H, Bucket_index, Sub_bucket_index).

next_non_equivalent_value(H, Bucket_index, Sub_bucket_index) ->
    lowest_equivalent_value(H, Bucket_index, Sub_bucket_index)
        + size_of_equivalent_value_range(H, Bucket_index, Sub_bucket_index).

median_equivalent_value(H, Bucket_index, Sub_bucket_index) ->
    lowest_equivalent_value(H, Bucket_index, Sub_bucket_index)
        + (size_of_equivalent_value_range(H, Bucket_index, Sub_bucket_index) bsr 1).

size_of_equivalent_value_range(H, Bucket_index, Sub_bucket_index) ->
    Adjusted_bucket_index =
        case Sub_bucket_index >= H#hist.sub_bucket_count of
            true -> Bucket_index + 1;
            false -> Bucket_index
        end,
    1 bsl (H#hist.unit_magnitude + Adjusted_bucket_index).


-record(it_state,
        {index = -1,
         count_at_index = 0,
         count_to_index = 0
        }).

-record(it,
        {h :: #hist{},
         total_count,
         counts,
         state = #it_state{} :: #it_state{}
        }).

iterator(H) ->
    Counts = get_counts(H),
    #it{h = H, counts = Counts, total_count = element(?TOTAL_COUNT_INDEX, Counts)}.
do_total_count(It) ->
    It#it.total_count.

do_max(It) ->
    MaxIndex = do_max_loop(It, 0, 0, 0),
    {MaxBucketIndex, MaxSubBucketIndex} =
        get_bucket_indexes_from_index(It#it.h, MaxIndex),
    %% The NIF uses an old version of the c code which calls lowest.
    %% In newer version of HdrHistogram_c hdr_max was refactorred and
    %% besides other changes it uses highest.

    %%highest_equivalent_value(It#it.h, MaxValue).
    lowest_equivalent_value(It#it.h, MaxBucketIndex, MaxSubBucketIndex).

do_max_loop(It, Index, CountToIndex0, Max0) ->
    CountAtIndex = count_at_index(It, Index),
    CountToIndex = CountToIndex0 + CountAtIndex,
    case CountToIndex >= It#it.total_count of
        true -> Index;
        false ->
          do_max_loop(It, Index + 1, CountToIndex, Max0)
    end.

do_min(It) ->
    case It#it.total_count of
        0 -> 0;
        _ ->
            MinIndex = do_min_loop(It, 0),
            {MinBucketIndex, MinSubBucketIndex} =
                get_bucket_indexes_from_index(It#it.h, MinIndex),
            lowest_equivalent_value(It#it.h, MinBucketIndex, MinSubBucketIndex)
    end.

do_min_loop(It, Index) ->
  Count_at_index = count_at_index(It, Index),
  case Count_at_index =/= 0 of
      true ->
          Index;
      false ->
          do_min_loop(It, Index + 1)
  end.

do_mean(It) ->
    case It#it.total_count =:= 0 of
        true -> 0;
        false ->
            Total = do_mean_loop(It, 0, 0, 0),
            Total / It#it.total_count
    end.

do_mean_loop(It, Index, CountToIndex, Total0) ->
    case CountToIndex >= It#it.total_count of
        true -> Total0;
        false ->
            CountAtIndex = count_at_index(It, Index),
            Total =
                case CountAtIndex of
                    0 -> Total0;
                    N ->
                        {BucketIndex, SubBucketIndex} =
                            get_bucket_indexes_from_index(It#it.h, Index),
                        Total0 + N * median_equivalent_value(
                                       It#it.h,
                                       BucketIndex,
                                       SubBucketIndex)
                end,
            do_mean_loop(It, Index + 1, CountToIndex + CountAtIndex, Total)
    end.

do_value_at_quantile(It, Q) ->
    Count_at_percetile = round(Q / 100 * It#it.total_count),
    H = It#it.h,

    Index = do_quantile_loop(It, 0, 0, Count_at_percetile),
    {BucketIndex, SubBucketIndex} =
        get_bucket_indexes_from_index(H, Index),
    highest_equivalent_value(H, BucketIndex, SubBucketIndex).

do_quantile_loop(It, Index, CountToIndex0, CountAtPercentile) ->
    CountAtIndex = count_at_index(It, Index),
    CountToIndex = CountToIndex0 + CountAtIndex,
    case CountToIndex >= CountAtPercentile of
        true -> Index;
        false ->

            do_quantile_loop(It, Index + 1, CountToIndex, CountAtPercentile)
    end.

count_at_index(It, Index) ->
    %% 1 is the name
    %% 2 is the total_count
    %% the real count buckets start at 3
    %% Index is zero based
    element(Index + ?TOTAL_COUNT_INDEX + 1, It#it.counts).

%% ceil/1 and floor/1 were introduced in OTP 20
-ifdef(ceil_floor).

int_ceil(F) ->
    erlang:ceil(F).

int_floor(F) ->
    erlang:floor(F).

-else.

int_ceil(F) ->
    R = round(F),
    if R < F -> R + 1;
       true -> R
    end.

int_floor(F) ->
    R = round(F),
    if R > F -> R - 1;
       true -> R
    end.

-endif.
