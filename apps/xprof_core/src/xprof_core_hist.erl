-module(xprof_core_hist).

-export([new/4,
         record/3,
         reset/1,
         delete/1,

         max/1
        ]).

-define(TABLE, ?MODULE).

-define(total_count_index, 2).

-record(hist,
        {%% field names from elixir
         name, registrar, bucket_count, counts_length, unit_magnitude,
         sub_bucket_mask, sub_bucket_count, sub_bucket_half_count,
         sub_bucket_half_count_magnitude, template

         %% field names from C
         %% lowest_trackable_value,
         %% highest_trackable_value,
         %% unit_magnitude,
         %% significant_figures,
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

new(Name, Min, Max, Precision) when Min > 0 andalso Max > Min
                                    andalso 1 =< Precision andalso Precision =< 5 ->
    ets:new(?TABLE, [set, public, named_table, {write_concurrency, true}]),

    Largest_value_with_single_unit_resolution = 2 * math:pow(10, Precision),
    Sub_bucket_count_magnitude = round(float_ceil(math:log2(Largest_value_with_single_unit_resolution))),

    Sub_bucket_half_count_magnitude =
        case Sub_bucket_count_magnitude < 1 of
            true -> 1;
            false -> Sub_bucket_count_magnitude - 1
        end,

    Unit_magnitude =
        case round(float_floor(math:log2(Min))) of
            N when N < 0 -> 0;
            N -> N
        end,

    Sub_bucket_count = round(math:pow(2, Sub_bucket_half_count_magnitude + 1)),
    Sub_bucket_half_count = round(Sub_bucket_count / 2),
    Sub_bucket_mask = (Sub_bucket_count - 1) bsl Unit_magnitude,

    Bucket_count = calculate_bucket_count(Sub_bucket_count bsl Unit_magnitude, Max, 1),
    Counts_length = round((Bucket_count + 1) * (Sub_bucket_count / 2)),

    %%template = case template do
    %%  false -> nil
    %%  true -> create_row(name, name, counts_length)
    %%end

    #hist{
       name = Name,
       %%template = Template,
       %%registrar = Registrar,
       bucket_count = Bucket_count,
       counts_length = Counts_length,
       unit_magnitude = Unit_magnitude,
       sub_bucket_mask = Sub_bucket_mask,
       sub_bucket_count = Sub_bucket_count,
       sub_bucket_half_count = Sub_bucket_half_count,
       sub_bucket_half_count_magnitude = Sub_bucket_half_count_magnitude
      }.




record(H, Value, N) when is_integer(Value), is_integer(N), N > 0 ->
    Index = get_value_index(H, Value),
    case Index < 0 orelse H#hist.counts_length =< Index of
        true ->
            {error, outside_of_range};
        false ->
            ets:update_counter(?TABLE,
                               H#hist.name,
                               [{?total_count_index, N},
                                {Index + ?total_count_index + 1, N}]),
            ok
    end.

reset(H) ->
    ets:insert(?TABLE, create_row(H#hist.name, nill, H#hist.counts_length)),
    ok.

delete(H) ->
    ets:delete(?TABLE, H#hist.name),
    ok.

max(H) ->
    do_max(iterator(H)).

%% Helper functions

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

create_row(Name, _Template, Count) ->
    %% +1 for the total_count that we'll store at the start
    list_to_tuple([Name | lists:duplicate(Count + 1, 0)]).

get_counts(H) ->
    case ets:lookup(?TABLE, H#hist.name) of
        [] ->
            undefined;
        [Counts] ->
            Counts
    end.

value_from_index(H, Bucket_index, Sub_bucket_index) ->
    Sub_bucket_index bsl (Bucket_index + H#hist.unit_magnitude).

highest_equivalent_value(H, Value) ->
    next_non_equivalent_value(H, Value) - 1.

lowest_equivalent_value(H, Value) ->
    {Bucket_index, Sub_bucket_index} = get_bucket_indexes(H, Value),
    lowest_equivalent_value(H, Bucket_index, Sub_bucket_index).

lowest_equivalent_value(H, Bucket_index, Sub_bucket_index) ->
    value_from_index(H, Bucket_index, Sub_bucket_index).

next_non_equivalent_value(H, Value) ->
    {Bucket_index, Sub_bucket_index} = get_bucket_indexes(H, Value),
    lowest_equivalent_value(H, Bucket_index, Sub_bucket_index)
        + size_of_equivalent_value_range(H, Bucket_index, Sub_bucket_index).

median_equivalent_value(H, Value) ->
    {Bucket_index, Sub_bucket_index} = get_bucket_indexes(H, Value),
    lowest_equivalent_value(H, Bucket_index, Sub_bucket_index)
        + (size_of_equivalent_value_range(H, Bucket_index, Sub_bucket_index) bsr 1).

size_of_equivalent_value_range(H, Bucket_index, Sub_bucket_index) ->
    Adjusted_bucket_index =
        case Sub_bucket_index >= H#hist.sub_bucket_count of
            true -> Bucket_index + 1;
            false -> Bucket_index
        end,
    1 bsl (H#hist.unit_magnitude + Adjusted_bucket_index).

float_ceil(F) ->
    math:ceil(F).

float_floor(F) ->
    math:floor(F).

-record(it,
        {h :: #hist{},
         total_count, counts,
         bucket_index = 0, sub_bucket_index = -1, count_at_index = 0,
         count_to_index = 0, value_from_index = 0, highest_equivalent_value = 0
        }).

it_reset(It) ->
    It#it{
      bucket_index = 0, sub_bucket_index = -1, count_at_index = 0,
      count_to_index = 0, value_from_index = 0, highest_equivalent_value = 0
     }.

iterator(H) ->
    Counts = get_counts(H),
    #it{h = H, counts = Counts, total_count = element(?total_count_index, Counts)}.

do_max(It) ->
    MaxValue = enum_reduce(It, 0,
                           fun(It0, Max0) ->
                                      case It0#it.count_at_index =:= 0 of
                                          true -> Max0;
                                          false -> It0#it.highest_equivalent_value
                                      end
                           end),
    highest_equivalent_value(It#it.h, MaxValue).

enum_reduce(Enum, Acc, F) ->
    {_, Res} = it_reduce(Enum, {cont, Acc},
                         fun(X, Acc0) -> {cont, F(X, Acc0)} end),
    Res.
                                       

it_reduce(It, {cont, Acc}, F) ->
    case It#it.count_to_index >= It#it.total_count of
        true -> {done, Acc};
        false -> it_do_reduce(It, Acc, F)
    end.

it_do_reduce(It, Acc, F) ->
    H = It#it.h,
    Sub_bucket_index = It#it.sub_bucket_index + 1,
    {It3, Bucket_index3, Sub_bucket_index3} =
        case Sub_bucket_index >= H#hist.sub_bucket_count of
            true ->
                Bucket_index = It#it.bucket_index + 1,
                Sub_bucket_index2 = H#hist.sub_bucket_half_count,
                It2 = It#it{bucket_index = Bucket_index,
                            sub_bucket_index = Sub_bucket_index2},
                {It2, Bucket_index, Sub_bucket_index2};
            false ->
                It2 = It#it{sub_bucket_index = Sub_bucket_index},
                {It2, It2#it.bucket_index, Sub_bucket_index}
        end,

    case Bucket_index3 >= H#hist.bucket_count of
        true -> {done, Acc};
        false ->
            Count_at_index =
                count_at_index(It3, Bucket_index3, Sub_bucket_index3),
            Value_from_index = value_from_index(H, Bucket_index3, Sub_bucket_index3),
                It4 = It3#it{
                        count_at_index = Count_at_index,
                        value_from_index = Value_from_index,
                        count_to_index = It3#it.count_to_index + Count_at_index,
                        highest_equivalent_value =
                            highest_equivalent_value(H, Value_from_index)
                       },
            it_reduce(It4, F(It4, Acc), F)
    end.

count_at_index(It, Bucket_index, Sub_bucket_index) ->
    Index = get_count_index(It#it.h, Bucket_index, Sub_bucket_index),
    %% 1 is the name
    %% 2 is the total_count
    %% the real count buckets start at 3
    %% Index is zero based
    element(Index + ?total_count_index + 1, It#it.counts).
