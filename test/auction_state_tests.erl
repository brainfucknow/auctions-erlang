-module(auction_state_tests).
-include_lib("eunit/include/eunit.hrl").

sample_starts_at() -> 100.
sample_ends_at() -> 200.
sample_bid_time() -> 150.

base_state() ->
    english_auction:new(#{ends_at => sample_ends_at()}).

has_ended({ended, _}) -> true;
has_ended(_) -> false.

increment_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
      {"can increment twice", fun can_increment_twice/0},
      {"wont end just after start", fun wont_end_just_after_start/0},
      {"wont end just before end", fun wont_end_just_before_end/0},
      {"wont end just before start", fun wont_end_just_before_start/0},
      {"will have ended just after end", fun will_have_ended_just_after_end/0}
     ]}.

can_increment_twice() ->
    Base = base_state(),
    S = english_auction:inc(sample_bid_time(), Base),
    S2 = english_auction:inc(sample_bid_time(), S),
    ?assertEqual(S, S2).

wont_end_just_after_start() ->
    Base = base_state(),
    %% sampleStartsAt + 1
    Time = sample_starts_at() + 1,
    State = english_auction:inc(Time, Base),
    ?assertEqual(false, has_ended(State)).

wont_end_just_before_end() ->
    Base = base_state(),
    %% sampleEndsAt - 1
    Time = sample_ends_at() - 1,
    State = english_auction:inc(Time, Base),
    ?assertEqual(false, has_ended(State)).

wont_end_just_before_start() ->
    Base = base_state(),
    %% sampleStartsAt - 1
    Time = sample_starts_at() - 1,
    State = english_auction:inc(Time, Base),
    ?assertEqual(false, has_ended(State)).

will_have_ended_just_after_end() ->
    Base = base_state(),
    %% sampleEndsAt + 1
    Time = sample_ends_at() + 1,
    State = english_auction:inc(Time, Base),
    ?assertEqual(true, has_ended(State)).
