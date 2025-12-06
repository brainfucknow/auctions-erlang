-module(blind_auction_tests).
-include_lib("eunit/include/eunit.hrl").

%% Sample Data
sample_ends_at() -> 100.
bid1() -> #{amount => 10, user => <<"buyer1">>}.
bid2() -> #{amount => 20, user => <<"buyer2">>}.

blind_auction_test() ->
    BlindAuction = blind_auction:new(#{type => blind, ends_at => sample_ends_at()}),

    %% 1. Add bid to empty state
    {State1, Result1} = blind_auction:add_bid(bid1(), BlindAuction),
    ?assertEqual(ok, Result1),

    %% 2. Add second bid
    {State2, Result2} = blind_auction:add_bid(bid2(), State1),
    ?assertEqual(ok, Result2),

    %% 2.5 Increment time but not yet ended
    StateRunning = blind_auction:inc(sample_ends_at() - 1, State2),
    %% Should still be running state (record state, not tuple {ended, ...})
    ?assertMatch({state, _, _, _}, StateRunning),

    %% 3. End auction
    %% Haskell: S.inc sampleEndsAt stateWith2Bids
    StateEnded = blind_auction:inc(sample_ends_at(), State2),

    %% Haskell: stateEndedAfterTwoBids `shouldBe` Left (SB.DisclosingBids [ bid2, bid1 ] sampleEndsAt SB.Blind)
    %% We check if it is in the ended state structure we defined.
    ?assertMatch({ended, _}, StateEnded),
    {ended, EndedState} = StateEnded,
    %% Check bids are present (order might matter depending on implementation, here we prepended)
    %% [bid2, bid1]
    {ended_state, _Type, Bids, EndsAt} = EndedState,
    ?assertEqual([bid2(), bid1()], Bids),
    ?assertEqual(sample_ends_at(), EndsAt),

    %% 4. Get winner
    Winner = blind_auction:try_get_amount_and_winner(StateEnded),
    ?assertEqual({ok, {20, <<"buyer2">>}}, Winner).
