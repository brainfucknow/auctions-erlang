-module(blind_auction).
-export([new/1, add_bid/2, inc/2, try_get_amount_and_winner/1]).

-record(state, {
    type = blind :: atom(),
    ends_at :: integer(),
    bids = [] :: [map()]
}).

-record(ended_state, {
    type = blind :: atom(),
    bids = [] :: [map()],
    ends_at :: integer()
}).

-type state() :: #state{} | {ended, #ended_state{}}.

-spec new(map()) -> state().
new(#{ends_at := EndsAt} = Opts) ->
    Type = maps:get(type, Opts, blind),
    #state{ends_at = EndsAt, type = Type}.

-spec add_bid(map(), state()) -> {state(), ok | {error, term()}}.
add_bid(Bid, State = #state{bids = Bids}) ->
    {State#state{bids = [Bid | Bids]}, ok};
add_bid(_Bid, State) ->
    {State, {error, auction_ended}}.

-spec inc(integer(), state()) -> state().
inc(Time, #state{ends_at = EndsAt, bids = Bids, type = Type}) when Time >= EndsAt ->
    {ended, #ended_state{bids = Bids, ends_at = EndsAt, type = Type}};
inc(_Time, State) ->
    State.

-spec try_get_amount_and_winner(state()) -> {ok, {integer(), binary()}} | undefined.
try_get_amount_and_winner({ended, #ended_state{bids = Bids, type = Type}}) ->
    case Bids of
        [] -> undefined;
        _ ->
            SortedBids = lists:sort(fun(A, B) -> maps:get(amount, A) > maps:get(amount, B) end, Bids),
            Winner = hd(SortedBids),
            #{amount := Amount, user := User} = Winner,
            FinalAmount = case Type of
                vickrey ->
                    case SortedBids of
                        [_Winner, Second | _] -> maps:get(amount, Second);
                        [_Winner] -> Amount
                    end;
                _ -> Amount
            end,
            {ok, {FinalAmount, User}}
    end;
try_get_amount_and_winner(_) ->
    undefined.
