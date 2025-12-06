-module(serialization_tests).
-include_lib("eunit/include/eunit.hrl").

read_commands_test() ->
    {ok, Cmds} = auction_serialization:read_jsonl("test/samples/sample-commands.jsonl"),
    ?assert(length(Cmds) > 0).

deserialize_type_test() ->
    Json = <<"\"English|0|0|0\"">>,
    Decoded = json:decode(Json),
    Type = auction_serialization:decode_auction_type(Decoded),
    ?assertEqual({english, 0, 0, 0}, Type).

read_add_auction_test() ->
    Json =
        <<"{\"$type\":\"AddAuction\",\"at\":\"2020-05-17T08:15:16.464Z\",\"auction\":{\"id\":1,\"startsAt\":\"2018-12-01T10:00:00.000Z\",\"title\":\"Some auction\",\"expiry\":\"2020-05-18T10:00:00.000Z\",\"user\":\"BuyerOrSeller|a1|Test\",\"type\":\"English|0|0|0\",\"currency\":\"VAC\"}}">>,
    Decoded = auction_serialization:decode(Json),
    ?assertMatch({add_auction, _, _}, Decoded),
    {add_auction, At, Auction} = Decoded,
    ?assertEqual(<<"2020-05-17T08:15:16.464Z">>, At),
    ?assertEqual(1, maps:get(id, Auction)),
    ?assertEqual({english, 0, 0, 0}, maps:get(type, Auction)).

read_place_bid_test() ->
    Json =
        <<"{\"$type\":\"PlaceBid\",\"at\":\"2020-05-17T08:15:22.948Z\",\"bid\":{\"id\":\"32e692cc3fdb451da9647d6eeca5b2e3\",\"auction\":1,\"user\":\"BuyerOrSeller|a2|Buyer\",\"amount\":11,\"at\":\"2020-05-17T08:15:22.940Z\"}}">>,
    Decoded = auction_serialization:decode(Json),
    ?assertMatch({place_bid, _, _}, Decoded),
    {place_bid, At, Bid} = Decoded,
    ?assertEqual(<<"2020-05-17T08:15:22.948Z">>, At),
    ?assertEqual(11, maps:get(amount, Bid)).

write_add_auction_test() ->
    Auction = #{
        id => 1,
        starts_at => <<"2016-01-01T08:28:00.607875Z">>,
        title => <<"auction">>,
        expiry => <<"2016-02-01T08:28:00.607875Z">>,
        user => #{id => <<"Sample_Seller">>, name => <<"Seller">>},
        type => vickrey,
        currency => <<"SEK">>
    },
    Command = {add_auction, <<"2016-01-01T08:28:00.607875Z">>, Auction},
    Encoded = auction_serialization:encode(Command),
    %% Decode back to check equality
    Decoded = auction_serialization:decode(iolist_to_binary(Encoded)),
    {add_auction, _, DecodedAuction} = Decoded,
    ?assertEqual(vickrey, maps:get(type, DecodedAuction)),
    ?assertEqual(<<"SEK">>, maps:get(currency, DecodedAuction)).

write_place_bid_test() ->
    Bid = #{
        id => <<"bid_id">>,
        auction_id => 1,
        user => #{id => <<"Buyer_1">>, name => <<"Buyer 1">>},
        amount => 10,
        at => <<"2016-01-01T08:28:00.607875000001Z">>
    },
    Command = {place_bid, <<"2016-02-01T07:28:00.607875Z">>, Bid},
    Encoded = auction_serialization:encode(Command),
    Decoded = auction_serialization:decode(iolist_to_binary(Encoded)),
    {place_bid, _, DecodedBid} = Decoded,
    ?assertEqual(10, maps:get(amount, DecodedBid)).

parse_amount_test() ->
    ?assertEqual({ok, {amount, <<"VAC">>, 0}}, auction_serialization:parse_amount(<<"VAC0">>)),
    ?assertEqual({ok, {amount, <<"SEK">>, 100}}, auction_serialization:parse_amount(<<"SEK100">>)).

format_amount_test() ->
    ?assertEqual(<<"VAC0">>, auction_serialization:format_amount({amount, <<"VAC">>, 0})).
