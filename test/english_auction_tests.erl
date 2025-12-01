-module(english_auction_tests).
-include_lib("eunit/include/eunit.hrl").

sample_ends_at() -> 100.
bid1() -> #{amount => 10, user => <<"buyer1">>}.
bid2() -> #{amount => 20, user => <<"buyer2">>}.
bid_less_than_2() -> #{amount => 15, user => <<"buyer3">>}.
sample_bid() -> #{amount => 30, user => <<"buyer4">>}.

english_auction_test() ->
    %% Setup
    OptionsStr = "English|0|0|0",
    Auction = english_auction:new(#{ends_at => sample_ends_at(), options => OptionsStr}),
    
    %% 1. Can add bid to empty state
    {StateWith1Bid, Result1} = english_auction:add_bid(bid1(), Auction),
    ?assertEqual(ok, Result1),
    
    %% 2. Can add second bid
    {StateWith2Bids, Result2} = english_auction:add_bid(bid2(), StateWith1Bid),
    ?assertEqual(ok, Result2),
    
    %% 3. Can end (empty)
    EmptyEndedState = english_auction:inc(sample_ends_at(), Auction),
    ?assertMatch({ended, _}, EmptyEndedState),
    {ended, EndedRecEmpty} = EmptyEndedState,
    %% {ended_state, Bids, EndsAt, Options}
    ?assertMatch({ended_state, [], _, _}, EndedRecEmpty),

    %% 4. Ended with two bids
    StateEndedAfterTwoBids = english_auction:inc(sample_ends_at(), StateWith2Bids),
    ?assertMatch({ended, _}, StateEndedAfterTwoBids),
    {ended, EndedRecTwo} = StateEndedAfterTwoBids,
    %% Check bids are [bid2, bid1]
    {ended_state, BidsTwo, _, _} = EndedRecTwo,
    ?assertEqual([bid2(), bid1()], BidsTwo),

    %% 5. Cant bid after auction has ended
    {_, ErrAfterEnded} = english_auction:add_bid(sample_bid(), StateEndedAfterTwoBids),
    ?assertEqual({error, {auction_has_ended, 1}}, ErrAfterEnded),

    %% 6. Can get winner and price
    Winner = english_auction:try_get_amount_and_winner(StateEndedAfterTwoBids),
    ?assertEqual({ok, {20, <<"buyer2">>}}, Winner),

    %% 7. Can't place bid lower than highest bid
    {_, MaybeFail} = english_auction:add_bid(bid_less_than_2(), StateWith2Bids),
    ?assertEqual({error, {must_place_bid_over_highest_bid, 20}}, MaybeFail).

serialization_test() ->
    SampleTypStr = "English|0|0|0",
    Parsed = english_auction:parse_options(SampleTypStr),
    %% {options, Min, Reserve, Inc}
    ?assertMatch({options, 0, 0, 0}, Parsed),
    
    Serialized = english_auction:format_options(Parsed),
    ?assertEqual(SampleTypStr, Serialized),

    SampleWithValuesTypStr = "English|10|20|30",
    ParsedValues = english_auction:parse_options(SampleWithValuesTypStr),
    ?assertMatch({options, 10, 20, 30}, ParsedValues),
    
    SerializedValues = english_auction:format_options(ParsedValues),
    ?assertEqual(SampleWithValuesTypStr, SerializedValues).
