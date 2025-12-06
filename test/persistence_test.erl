-module(persistence_test).
-include_lib("eunit/include/eunit.hrl").

persistence_test() ->
    %% Clean up any previous run
    catch gen_server:stop(auction_store),
    file:delete("auction_store.dets"),
    
    %% Start the store
    {ok, Pid} = auction_store:start_link(),
    
    %% Create an auction
    Info = #{id => 1, title => <<"Test Auction">>, starts_at => <<"now">>, ends_at => <<"later">>, currency => <<"USD">>, type => {english, 0, 0, 0}},
    LogicState = english_auction:new(#{ends_at => 3000000000000}),
    ok = auction_store:create_auction(Info, english_auction, LogicState),
    
    %% Verify it's there
    ?assertMatch(#{<<"id">> := 1}, auction_store:get_auction(1)),
    
    %% Stop the store
    gen_server:stop(Pid),
    
    %% Start the store again
    {ok, Pid2} = auction_store:start_link(),
    
    %% Verify it's still there
    ?assertMatch(#{<<"id">> := 1}, auction_store:get_auction(1)),
    
    %% Clean up
    gen_server:stop(Pid2),
    file:delete("auction_store.dets").

persistence_bid_test() ->
    %% Clean up any previous run
    catch gen_server:stop(auction_store),
    file:delete("auction_store.dets"),
    
    %% Start the store
    {ok, Pid} = auction_store:start_link(),
    
    %% Create an auction
    Info = #{id => 1, title => <<"Test Auction">>, starts_at => <<"now">>, ends_at => <<"later">>, currency => <<"USD">>, type => {english, 0, 0, 0}},
    LogicState = english_auction:new(#{ends_at => 3000000000000}),
    ok = auction_store:create_auction(Info, english_auction, LogicState),
    
    %% Add a bid
    Bid = #{amount => 100, user => <<"User1">>, time => <<"now">>},
    ok = auction_store:add_bid(1, Bid),
    
    %% Stop the store
    gen_server:stop(Pid),
    
    %% Start the store again
    {ok, Pid2} = auction_store:start_link(),
    
    %% Verify the bid is there
    Auction = auction_store:get_auction(1),
    Bids = maps:get(<<"bids">>, Auction),
    ?assertMatch([#{<<"amount">> := 100, <<"bidder">> := <<"User1">>}], Bids),
    
    %% Clean up
    gen_server:stop(Pid2),
    file:delete("auction_store.dets").

config_test() ->
    %% Set a custom file path
    catch gen_server:stop(auction_store),
    CustomFile = "custom_store.dets",
    file:delete(CustomFile),
    application:set_env(auctions, dets_file, CustomFile),
    
    %% Start the store
    {ok, Pid} = auction_store:start_link(),
    
    %% Create an auction
    Info = #{id => 2, title => <<"Custom Auction">>, starts_at => <<"now">>, ends_at => <<"later">>, currency => <<"USD">>, type => {english, 0, 0, 0}},
    LogicState = english_auction:new(#{ends_at => 3000000000000}),
    ok = auction_store:create_auction(Info, english_auction, LogicState),
    
    %% Stop the store
    gen_server:stop(Pid),
    
    %% Verify the custom file exists
    ?assert(filelib:is_file(CustomFile)),
    
    %% Clean up
    file:delete(CustomFile),
    application:unset_env(auctions, dets_file).
