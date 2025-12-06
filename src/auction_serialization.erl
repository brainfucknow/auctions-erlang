-module(auction_serialization).
-export([decode/1, encode/1, read_jsonl/1]).
-export([decode_auction_type/1, encode_auction_type/1]).
-export([decode_user/1, encode_user/1]).
-export([parse_amount/1, format_amount/1]).
-export([decode_auction_req/1, decode_bid_req/1]).

decode(Json) when is_binary(Json) ->
    try
        Map = json:decode(Json),
        decode_map(Map)
    catch
        _:_ -> {error, invalid_json}
    end.

decode_map(#{<<"$type">> := <<"AddAuction">>} = Map) ->
    #{<<"at">> := At, <<"auction">> := AuctionMap} = Map,
    {add_auction, At, decode_auction(AuctionMap)};
decode_map(#{<<"$type">> := <<"PlaceBid">>} = Map) ->
    #{<<"at">> := At, <<"bid">> := BidMap} = Map,
    {place_bid, At, decode_bid(BidMap)};
decode_map(#{<<"$type">> := Type}) ->
    {error, {unknown_type, Type}};
decode_map(_) ->
    {error, missing_type_field}.

decode_auction(Map) ->
    #{
        <<"id">> := Id,
        <<"startsAt">> := StartsAt,
        <<"title">> := Title,
        <<"expiry">> := Expiry,
        <<"user">> := UserStr,
        <<"type">> := TypeStr,
        <<"currency">> := Currency
    } = Map,
    #{
        id => Id,
        starts_at => StartsAt,
        title => Title,
        expiry => Expiry,
        user => decode_user(UserStr),
        type => decode_auction_type(TypeStr),
        currency => Currency
    }.

decode_bid(Map) ->
    #{
        <<"id">> := Id,
        <<"auction">> := AuctionId,
        <<"user">> := UserStr,
        <<"amount">> := Amount,
        <<"at">> := At
    } = Map,
    #{
        id => Id,
        auction_id => AuctionId,
        user => decode_user(UserStr),
        amount => Amount,
        at => At
    }.

decode_user(Bin) ->
    case binary:split(Bin, <<"|">>, [global]) of
        [<<"BuyerOrSeller">>, Id, Name] -> #{id => Id, name => Name};
        _ -> Bin
    end.

decode_auction_type(Bin) ->
    case binary:split(Bin, <<"|">>, [global]) of
        [<<"English">>, Min, Reserve, Inc] ->
            {english, binary_to_integer(Min), binary_to_integer(Reserve), binary_to_integer(Inc)};
        [<<"Vickrey">>] ->
            vickrey;
        [<<"Blind">>] ->
            blind;
        _ ->
            Bin
    end.

encode({add_auction, At, Auction}) ->
    Map = #{
        <<"$type">> => <<"AddAuction">>,
        <<"at">> => At,
        <<"auction">> => encode_auction(Auction)
    },
    json:encode(Map);
encode({place_bid, At, Bid}) ->
    Map = #{
        <<"$type">> => <<"PlaceBid">>,
        <<"at">> => At,
        <<"bid">> => encode_bid(Bid)
    },
    json:encode(Map).

encode_auction(Auction) ->
    #{
        id := Id,
        starts_at := StartsAt,
        title := Title,
        expiry := Expiry,
        user := User,
        type := Type,
        currency := Currency
    } = Auction,
    #{
        <<"id">> => Id,
        <<"startsAt">> => StartsAt,
        <<"title">> => Title,
        <<"expiry">> => Expiry,
        <<"user">> => encode_user(User),
        <<"type">> => encode_auction_type(Type),
        <<"currency">> => Currency
    }.

encode_bid(Bid) ->
    #{
        id := Id,
        auction_id := AuctionId,
        user := User,
        amount := Amount,
        at := At
    } = Bid,
    #{
        <<"id">> => Id,
        <<"auction">> => AuctionId,
        <<"user">> => encode_user(User),
        <<"amount">> => Amount,
        <<"at">> => At
    }.

encode_user(#{id := Id, name := Name}) ->
    iolist_to_binary([<<"BuyerOrSeller|">>, Id, <<"|">>, Name]);
encode_user(Bin) when is_binary(Bin) -> Bin.

encode_auction_type({english, Min, Reserve, Inc}) ->
    iolist_to_binary([
        <<"English|">>,
        integer_to_integer(Min),
        <<"|">>,
        integer_to_integer(Reserve),
        <<"|">>,
        integer_to_integer(Inc)
    ]);
encode_auction_type(vickrey) ->
    <<"Vickrey">>;
encode_auction_type(blind) ->
    <<"Blind">>.

integer_to_integer(I) when is_integer(I) -> integer_to_binary(I);
integer_to_integer(B) when is_binary(B) -> B.

read_jsonl(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            Lines = binary:split(Binary, <<"\n">>, [global, trim]),
            {ok, [decode(Line) || Line <- Lines, Line =/= <<>>]};
        Error ->
            Error
    end.

parse_amount(Bin) ->
    case re:run(Bin, "^([A-Z]+)([0-9]+)$", [{capture, all_but_first, binary}]) of
        {match, [Currency, AmountStr]} when is_binary(Currency), is_binary(AmountStr) ->
            {ok, {amount, Currency, binary_to_integer(AmountStr)}};
        nomatch ->
            {error, invalid_amount_format};
        _ ->
            {error, invalid_amount_format}
    end.

format_amount({amount, Currency, Value}) ->
    iolist_to_binary([Currency, integer_to_binary(Value)]).

decode_auction_req(Json) when is_binary(Json) ->
    DecodedValue = json:decode(Json),
    Map = case DecodedValue of
        M when is_map(M) -> M;
        _ -> error(invalid_json_object)
    end,
    Id = maps:get(<<"id">>, Map),
    StartsAt = maps:get(<<"startsAt">>, Map),
    Title = maps:get(<<"title">>, Map),
    EndsAt = maps:get(<<"endsAt">>, Map),
    Currency = maps:get(<<"currency">>, Map, <<"VAC">>),
    Type =
        case maps:get(<<"type">>, Map, undefined) of
            undefined -> {english, 0, 0, 0};
            T -> decode_auction_type(T)
        end,
    #{
        id => Id,
        starts_at => StartsAt,
        title => Title,
        ends_at => EndsAt,
        currency => Currency,
        type => Type
    }.

decode_bid_req(Json) when is_binary(Json) ->
    DecodedValue = json:decode(Json),
    Map = case DecodedValue of
        M when is_map(M) -> M;
        _ -> error(invalid_json_object)
    end,
    Amount = maps:get(<<"amount">>, Map),
    #{amount => Amount}.
