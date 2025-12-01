-module(api_serialization_tests).
-include_lib("eunit/include/eunit.hrl").

auction_deserialization_test() ->
    Json = <<"{\"id\":1,\"startsAt\":\"2016-01-01T00:00:00.000Z\",\"endsAt\":\"2016-02-01T00:00:00.000Z\",\"title\":\"First auction\"}">>,
    Decoded = auction_serialization:decode_auction_req(Json),
    Expected = #{
        id => 1,
        title => <<"First auction">>,
        starts_at => <<"2016-01-01T00:00:00.000Z">>,
        ends_at => <<"2016-02-01T00:00:00.000Z">>,
        currency => <<"VAC">>,
        type => {english, 0, 0, 0}
    },
    ?assertEqual(Expected, Decoded).

bid_deserialization_test() ->
    Json = <<"{\"amount\":10}">>,
    Decoded = auction_serialization:decode_bid_req(Json),
    Expected = #{
        amount => 10
    },
    ?assertEqual(Expected, Decoded).
