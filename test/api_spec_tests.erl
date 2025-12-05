-module(api_spec_tests).
-include_lib("eunit/include/eunit.hrl").

-define(HOST, "http://localhost").

start_app() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jesse),
    application:load(auctions),
    
    %% Start the logic handler mock
    test_logic_handler:start_link(),
    test_logic_handler:reset(),

    %% Start the API server
    Port = 0, %% Random port
    {ok, _Pid} = auctions_server:start(api_test_listener, #{
        transport => tcp,
        transport_opts => #{socket_opts => [{port, Port}]},
        logic_handler => test_logic_handler
    }),
    ranch:get_port(api_test_listener).

stop_app() ->
    cowboy:stop_listener(api_test_listener),
    test_logic_handler:stop().

api_spec_test_() ->
    {setup,
     fun start_app/0,
     fun(_Port) -> stop_app() end,
     fun(Port) ->
        [
            {"add auction", fun() -> test_add_auction(Port) end},
            {"add bids to auction", fun() -> test_add_bids(Port) end}
        ]
     end}.

test_add_auction(Port) ->
    BaseUrl = ?HOST ++ ":" ++ integer_to_list(Port),
    
    Seller1 = <<"eyJzdWIiOiJhMSIsICJuYW1lIjoiVGVzdCIsICJ1X3R5cCI6IjAifQo=">>,
    FirstAuctionReq = <<"{\"id\":1,\"startsAt\":\"2018-01-01T10:00:00.000Z\",\"endsAt\":\"2019-01-01T10:00:00.000Z\",\"title\":\"First auction\", \"currency\":\"VAC\" }">>,

    %% 1. Possible to add auction
    {ok, {{_, 200, _}, _, Body1}} = httpc:request(post, 
        {BaseUrl ++ "/auctions", 
         [{"x-jwt-payload", binary_to_list(Seller1)}], 
         "application/json", 
         FirstAuctionReq}, 
        [], [{body_format, binary}]),
    
    Resp1 = json:decode(Body1),
    ?assertEqual(<<"AuctionAdded">>, maps:get(<<"$type">>, Resp1)),
    Auction1 = maps:get(<<"auction">>, Resp1),
    ?assertEqual(1, maps:get(<<"id">>, Auction1)),
    ?assertEqual(<<"First auction">>, maps:get(<<"title">>, Auction1)),

    %% 2. Not possible to add same auction twice
    {ok, {{_, 400, _}, _, Body2}} = httpc:request(post, 
        {BaseUrl ++ "/auctions", 
         [{"x-jwt-payload", binary_to_list(Seller1)}], 
         "application/json", 
         FirstAuctionReq}, 
        [], [{body_format, binary}]),
    ?assertEqual(<<"\"AuctionAlreadyExists 1\"">>, Body2),

    %% 3. Returns added auction
    {ok, {{_, 200, _}, _, Body3}} = httpc:request(get, 
        {BaseUrl ++ "/auctions/1", []}, 
        [], [{body_format, binary}]),
    Resp3 = json:decode(Body3),
    ?assertEqual(1, maps:get(<<"id">>, Resp3)),
    ?assertEqual([], maps:get(<<"bids">>, Resp3)),

    %% 4. Returns added auctions
    {ok, {{_, 200, _}, _, Body4}} = httpc:request(get, 
        {BaseUrl ++ "/auctions", []}, 
        [], [{body_format, binary}]),
    Resp4 = json:decode(Body4),
    ?assert(is_list(Resp4)),
    ?assertEqual(1, length(Resp4)),
    ok.

test_add_bids(Port) ->
    %% Reset state for this test suite or reuse?
    %% The Haskell spec runs `addAuctionOk` in `addBidSpec` too.
    %% So we need to ensure auction 1 exists.
    %% Since we run tests sequentially in setup, auction 1 exists from previous test.
    %% But to be safe/clean, we can rely on it or recreate.
    %% `test_logic_handler` is stateful.
    
    BaseUrl = ?HOST ++ ":" ++ integer_to_list(Port),
    Buyer1 = <<"eyJzdWIiOiJhMiIsICJuYW1lIjoiQnV5ZXIiLCAidV90eXAiOiIwIn0K">>,
    BidReq = <<"{\"amount\":11}">>,

    %% 1. Possible to add bid to auction
    {ok, {{_, 200, _}, _, Body1}} = httpc:request(post, 
        {BaseUrl ++ "/auctions/1/bids", 
         [{"x-jwt-payload", binary_to_list(Buyer1)}], 
         "application/json", 
         BidReq}, 
        [], [{body_format, binary}]),
    
    Resp1 = json:decode(Body1),
    ?assertEqual(<<"BidAccepted">>, maps:get(<<"$type">>, Resp1)),
    Bid1 = maps:get(<<"bid">>, Resp1),
    ?assertEqual(11, maps:get(<<"amount">>, Bid1)),
    ?assertEqual(<<"BuyerOrSeller|a2|Buyer">>, maps:get(<<"user">>, Bid1)),

    %% 2. Possible to see the added bids
    {ok, {{_, 200, _}, _, Body2}} = httpc:request(get, 
        {BaseUrl ++ "/auctions/1", []}, 
        [], [{body_format, binary}]),
    Resp2 = json:decode(Body2),
    Bids = maps:get(<<"bids">>, Resp2),
    ?assertEqual(1, length(Bids)),
    [Bid2] = Bids,
    ?assertEqual(11, maps:get(<<"amount">>, Bid2)),

    %% 3. Not possible to add bid to non existant auction
    {ok, {{_, 404, _}, _, Body3}} = httpc:request(post, 
        {BaseUrl ++ "/auctions/2/bids", 
         [{"x-jwt-payload", binary_to_list(Buyer1)}], 
         "application/json", 
         <<"{\"amount\":10}">>}, 
        [], [{body_format, binary}]),
    ?assertEqual(<<"\"Auction not found\"">>, Body3),
    ok.
