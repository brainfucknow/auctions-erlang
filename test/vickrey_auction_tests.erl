-module(vickrey_auction_tests).
-include_lib("eunit/include/eunit.hrl").

%% Sample Data
sample_ends_at() -> 100.
bid1() -> #{amount => 10, user => <<"buyer1">>}.
bid2() -> #{amount => 20, user => <<"buyer2">>}.

vickrey_auction_test() ->
    %% Initialize as Vickrey auction
    VickreyAuction = blind_auction:new(#{type => vickrey, ends_at => sample_ends_at()}),
    
    %% 1. Add bid to empty state
    {State1, Result1} = blind_auction:add_bid(bid1(), VickreyAuction),
    ?assertEqual(ok, Result1),
    
    %% 2. Add second bid
    {State2, Result2} = blind_auction:add_bid(bid2(), State1),
    ?assertEqual(ok, Result2),
    
    %% 3. End auction
    StateEnded = blind_auction:inc(sample_ends_at(), State2),
    ?assertMatch({ended, _}, StateEnded),

    %% 4. Get winner and price
    %% Winner should be buyer2 (highest bid 20), but price should be 10 (second highest bid)
    Winner = blind_auction:try_get_amount_and_winner(StateEnded),
    ?assertEqual({ok, {10, <<"buyer2">>}}, Winner).

blind_vs_vickrey_test() ->
    %% Blind Auction
    BlindAuction = blind_auction:new(#{type => blind, ends_at => sample_ends_at()}),
    {StateB1, _} = blind_auction:add_bid(bid1(), BlindAuction),
    {StateB2, _} = blind_auction:add_bid(bid2(), StateB1),
    StateBEnded = blind_auction:inc(sample_ends_at(), StateB2),
    ?assertEqual({ok, {20, <<"buyer2">>}}, blind_auction:try_get_amount_and_winner(StateBEnded)),

    %% Vickrey Auction
    VickreyAuction = blind_auction:new(#{type => vickrey, ends_at => sample_ends_at()}),
    {StateV1, _} = blind_auction:add_bid(bid1(), VickreyAuction),
    {StateV2, _} = blind_auction:add_bid(bid2(), StateV1),
    StateVEnded = blind_auction:inc(sample_ends_at(), StateV2),
    ?assertEqual({ok, {10, <<"buyer2">>}}, blind_auction:try_get_amount_and_winner(StateVEnded)).
