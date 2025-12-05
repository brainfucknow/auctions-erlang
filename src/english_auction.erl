-module(english_auction).
-export([new/1, add_bid/2, inc/2, try_get_amount_and_winner/1, parse_options/1, format_options/1, get_bids/1]).

-record(options, {
    min_bid = 0 :: integer(),
    reserve_price = 0 :: integer(),
    increment = 0 :: integer()
}).

-record(state, {
    ends_at :: integer(),
    bids = [] :: [map()],
    options = #options{} :: #options{}
}).

-record(ended_state, {
    bids = [] :: [map()],
    ends_at :: integer(),
    options :: #options{}
}).

-type state() :: #state{} | {ended, #ended_state{}}.
-type options() :: #options{}.

-spec new(map()) -> state().
new(#{ends_at := EndsAt} = Params) ->
    Options = case maps:get(options, Params, undefined) of
        undefined -> #options{};
        OptRecord when is_record(OptRecord, options) -> OptRecord;
        OptString when is_list(OptString) -> parse_options(OptString);
        OptBinary when is_binary(OptBinary) -> parse_options(binary_to_list(OptBinary))
    end,
    #state{ends_at = EndsAt, options = Options}.

-spec add_bid(map(), state()) -> {state(), ok | {error, term()}}.
add_bid(Bid = #{amount := Amount}, State = #state{bids = Bids, options = Options}) ->
    case validate_bid(Amount, Bids, Options) of
        ok ->
            {State#state{bids = [Bid | Bids]}, ok};
        {error, Reason} ->
            {State, {error, Reason}}
    end;
add_bid(_Bid, State) ->
    {State, {error, {auction_has_ended, 1}}}.

validate_bid(Amount, [], #options{min_bid = Min}) ->
    if Amount >= Min -> ok;
       true -> {error, {bid_too_low, Min}}
    end;
validate_bid(Amount, [#{amount := Highest} | _], #options{increment = Inc}) ->
    Required = if Inc > 0 -> Highest + Inc; true -> Highest + 1 end,
    if Amount >= Required -> ok;
       true -> {error, {must_place_bid_over_highest_bid, Highest}}
    end.

-spec inc(integer(), state()) -> state().
inc(Time, #state{ends_at = EndsAt, bids = Bids, options = Options}) when Time >= EndsAt ->
    {ended, #ended_state{bids = Bids, ends_at = EndsAt, options = Options}};
inc(_Time, State) ->
    State.

-spec try_get_amount_and_winner(state()) -> {ok, {integer(), binary()}} | undefined.
try_get_amount_and_winner({ended, #ended_state{bids = [#{amount := Amount, user := User} | _]}}) ->
    {ok, {Amount, User}};
try_get_amount_and_winner(_) ->
    undefined.

get_bids(#state{bids = Bids}) -> Bids;
get_bids({ended, #ended_state{bids = Bids}}) -> Bids.

-spec parse_options(string()) -> options().
parse_options(String) ->
    case string:tokens(String, "|") of
        ["English", MinStr, ReserveStr, IncStr] ->
            #options{
                min_bid = list_to_integer(MinStr),
                reserve_price = list_to_integer(ReserveStr),
                increment = list_to_integer(IncStr)
            };
        _ -> error(invalid_format)
    end.

-spec format_options(options()) -> string().
format_options(#options{min_bid = Min, reserve_price = Reserve, increment = Inc}) ->
    lists:flatten(io_lib:format("English|~p|~p|~p", [Min, Reserve, Inc])).
