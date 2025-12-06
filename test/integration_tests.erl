-module(integration_tests).
-include_lib("eunit/include/eunit.hrl").

-define(HOST, "http://localhost").

start_app() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jesse),
    application:load(auctions),

    %% Start the store manually since we are not starting the full app
    auction_store:start_link(),

    %% Start the API server with REAL logic handler
    Port = 0,
    {ok, _Pid} = auctions_server:start(integration_test_listener, #{
        transport => tcp,
        transport_opts => #{socket_opts => [{port, Port}]},
        logic_handler => auctions_logic_handler
    }),
    ranch:get_port(integration_test_listener).

stop_app() ->
    cowboy:stop_listener(integration_test_listener),
    %% Stop auction_store if it was started by auctions_server
    catch gen_server:stop(auction_store).

integration_test_() ->
    {setup, fun start_app/0, fun(_Port) -> stop_app() end, fun(Port) ->
        [
            {"Full flow test", fun() -> test_flow(Port) end}
        ]
    end}.

test_flow(Port) ->
    BaseUrl = ?HOST ++ ":" ++ integer_to_list(Port),

    %% Create Auction
    AuctionReq = #{
        <<"id">> => 1,
        <<"startsAt">> => <<"2025-01-01T10:00:00.000Z">>,
        <<"endsAt">> => <<"2025-12-31T10:00:00.000Z">>,
        <<"title">> => <<"Integration Test Auction">>,
        <<"currency">> => <<"VAC">>,
        <<"type">> => <<"English|0|0|1">>
    },
    AuctionReqJson = json:encode(AuctionReq),

    {ok, {{_, 200, _}, _, _}} = httpc:request(
        post,
        {BaseUrl ++ "/auctions", [], "application/json", AuctionReqJson},
        [],
        []
    ),

    %% Add Bid
    %% Need JWT header
    JwtPayload = base64:encode(
        iolist_to_binary(json:encode(#{<<"sub">> => <<"u1">>, <<"name">> => <<"bidder1">>}))
    ),
    BidReq = #{<<"amount">> => 10},
    BidReqJson = json:encode(BidReq),

    {ok, {{_, 200, _}, _, BidRespBody}} = httpc:request(
        post,
        {
            BaseUrl ++ "/auctions/1/bids",
            [{"x-jwt-payload", binary_to_list(JwtPayload)}],
            "application/json",
            BidReqJson
        },
        [],
        []
    ),

    BidResp = json:decode(list_to_binary(BidRespBody)),
    ?assertEqual(10, maps:get(<<"amount">>, BidResp)),
    ?assertEqual(<<"bidder1">>, maps:get(<<"bidder">>, BidResp)),

    %% Get Auctions
    {ok, {{_, 200, _}, _, AuctionsBody}} = httpc:request(get, {BaseUrl ++ "/auctions", []}, [], []),
    Auctions = json:decode(list_to_binary(AuctionsBody)),
    [Auction] = Auctions,
    ?assertEqual(1, maps:get(<<"id">>, Auction)),
    ?assertEqual(<<"Integration Test Auction">>, maps:get(<<"title">>, Auction)),

    %% Check bids in auction
    Bids = maps:get(<<"bids">>, Auction),
    ?assertEqual(1, length(Bids)),
    [Bid] = Bids,
    ?assertEqual(10, maps:get(<<"amount">>, Bid)),
    ?assertEqual(<<"bidder1">>, maps:get(<<"bidder">>, Bid)).
