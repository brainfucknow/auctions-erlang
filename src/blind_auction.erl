-module(blind_auction).
-export([new/1, add_bid/2, inc/2, try_get_amount_and_winner/1]).

-record(state, {
    ends_at :: integer(),
    bids = [] :: [map()]
}).

-record(ended_state, {
    bids = [] :: [map()],
    ends_at :: integer()
}).

-type state() :: #state{} | {ended, #ended_state{}}.

-spec new(map()) -> state().
new(#{ends_at := EndsAt}) ->
    #state{ends_at = EndsAt}.

-spec add_bid(map(), state()) -> {state(), ok | {error, term()}}.
add_bid(Bid, State = #state{bids = Bids}) ->
    {State#state{bids = [Bid | Bids]}, ok};
add_bid(_Bid, State) ->
    {State, {error, auction_ended}}.

-spec inc(integer(), state()) -> state().
inc(Time, #state{ends_at = EndsAt, bids = Bids}) when Time >= EndsAt ->
    {ended, #ended_state{bids = Bids, ends_at = EndsAt}};
inc(_Time, State) ->
    State.

-spec try_get_amount_and_winner(state()) -> {ok, {integer(), binary()}} | undefined.
try_get_amount_and_winner({ended, #ended_state{bids = Bids}}) ->
    %% Find highest bid. If tie, take the one that appears first?
    %% Haskell spec: [bid2, bid1]. bid2 is 20, bid1 is 10.
    %% We need to sort or find max.
    case Bids of
        [] -> undefined;
        _ ->
            #{amount := Amount, user := User} = find_winner(Bids),
            {ok, {Amount, User}}
    end;
try_get_amount_and_winner(_) ->
    undefined.

find_winner(Bids) ->
    lists:foldl(fun(Bid, MaxBid) ->
        case {Bid, MaxBid} of
            {#{amount := A}, #{amount := MaxA}} when A > MaxA -> Bid;
            {_, _} -> MaxBid
        end
    end, hd(Bids), tl(Bids)).
