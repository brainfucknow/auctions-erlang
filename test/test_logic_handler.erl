-module(test_logic_handler).
-behaviour(openapi_logic_handler).

-export([accept_callback/4, provide_callback/4, api_key_callback/2]).
-export([start_link/0, stop/0, reset/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("kernel/include/logger.hrl").

%% State for the mock DB
-record(state, {
    auctions = #{} :: map()
}).

%% API to control the mock DB
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

reset() ->
    gen_server:call(?MODULE, reset).

%% openapi_logic_handler callbacks

api_key_callback(_OperationID, _ApiKey) ->
    {true, #{}}.

accept_callback('createAuction', 'create_auction', Req, Context) ->
    ValidatorState = openapi_api:prepare_validator(),
    case openapi_api:populate_request('create_auction', Req, ValidatorState) of
        {ok, Model, Req1} ->
            AuctionReq = maps:get('CreateAuctionModel', Model),
            case gen_server:call(?MODULE, {create_auction, AuctionReq}) of
                {ok, CreatedAuction} ->
                    %% Return 200 OK with the created auction event
                    %% Haskell spec expects "AuctionAdded" event
                    RespBody = encode_auction_added(CreatedAuction),
                    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, RespBody, Req1),
                    {stop, Req2, Context};
                {error, exists} ->
                    %% Haskell spec expects 400 "AuctionAlreadyExists 1"
                    Id = maps:get(<<"id">>, AuctionReq),
                    Msg = iolist_to_binary(io_lib:format("\"AuctionAlreadyExists ~p\"", [Id])),
                    Req2 = cowboy_req:reply(400, #{}, Msg, Req1),
                    {stop, Req2, Context}
            end;
        {error, Reason, Req1} ->
            Req2 = cowboy_req:reply(400, #{}, io_lib:format("~p", [Reason]), Req1),
            {stop, Req2, Context}
    end;

accept_callback('createBid', 'add_bid', Req, Context) ->
    ValidatorState = openapi_api:prepare_validator(),
    case openapi_api:populate_request('add_bid', Req, ValidatorState) of
        {ok, Model, Req1} ->
            AuctionId = maps:get('auctionId', Model),
            BidReq = maps:get('CreateBidModel', Model),
            %% Extract user from JWT if possible, but for now use what's in the bid or mock it
            %% Haskell spec sends x-jwt-payload.
            %% The bid request in Haskell spec is {"amount":11}.
            %% The response is BidAccepted event.
            
            %% We need to get the user from the header manually since openapi_auth is bypassed
            Headers = cowboy_req:headers(Req1),
            User = case maps:get(<<"x-jwt-payload">>, Headers, undefined) of
                undefined -> <<"unknown">>;
                Jwt -> 
                    %% Simple decode for test
                    try 
                        Decoded = base64:decode(Jwt),
                        Json = json:decode(Decoded),
                        Sub = maps:get(<<"sub">>, Json),
                        Name = maps:get(<<"name">>, Json),
                        %% Format: BuyerOrSeller|id|Name
                        iolist_to_binary(io_lib:format("BuyerOrSeller|~s|~s", [Sub, Name]))
                    catch _:_ -> <<"unknown">> end
            end,

            case gen_server:call(?MODULE, {add_bid, AuctionId, BidReq, User}) of
                {ok, BidEvent} ->
                    RespBody = encode_bid_accepted(BidEvent),
                    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, RespBody, Req1),
                    {stop, Req2, Context};
                {error, not_found} ->
                    Req2 = cowboy_req:reply(404, #{}, <<"\"Auction not found\"">>, Req1),
                    {stop, Req2, Context};
                {error, Reason} ->
                    Req2 = cowboy_req:reply(400, #{}, io_lib:format("~p", [Reason]), Req1),
                    {stop, Req2, Context}
            end;
        {error, Reason, Req1} ->
            Req2 = cowboy_req:reply(400, #{}, io_lib:format("~p", [Reason]), Req1),
            {stop, Req2, Context}
    end.

provide_callback('getAuctions', 'get_auctions', Req, Context) ->
    Auctions = gen_server:call(?MODULE, get_auctions),
    RespBody = json:encode([format_auction(A) || A <- Auctions]),
    {RespBody, Req, Context};

provide_callback('createAuction', 'get_auction', Req, Context) ->
    %% Note: 'createAuction' class is used for get_auction in openapi_router?
    %% Let's check openapi_router.erl.
    %% get_auction handler is openapi_get_auctions_handler.
    %% openapi_get_auctions_handler uses class 'getAuctions' (likely).
    %% Wait, openapi_get_auctions_handler.erl says: -type class() :: 'getAuctions'.
    %% So this clause might be wrong if I use 'createAuction'.
    %% But let's implement for 'getAuctions'.
    provide_callback('getAuctions', 'get_auction', Req, Context);

provide_callback('getAuctions', 'get_auction', Req, Context) ->
    ValidatorState = openapi_api:prepare_validator(),
    case openapi_api:populate_request('get_auction', Req, ValidatorState) of
        {ok, Model, Req1} ->
            AuctionId = maps:get('auctionId', Model),
            case gen_server:call(?MODULE, {get_auction, AuctionId}) of
                {ok, Auction} ->
                    RespBody = json:encode(format_auction(Auction)),
                    {RespBody, Req1, Context};
                {error, not_found} ->
                    %% Cowboy REST doesn't easily support 404 in provide_callback without some tricks
                    %% or returning empty body and setting response code manually?
                    %% Actually, if resource doesn't exist, we should have returned false in resource_exists.
                    %% But openapi handlers are generated to return true.
                    %% So we have to handle it here.
                    Req2 = cowboy_req:reply(404, #{}, <<"\"Auction not found\"">>, Req1),
                    {stop, Req2, Context}
            end;
        {error, _Reason, Req1} ->
             Req2 = cowboy_req:reply(400, #{}, <<"Bad Request">>, Req1),
             {stop, Req2, Context}
    end.


%% gen_server implementation

init([]) ->
    {ok, #state{}}.

handle_call(reset, _From, _State) ->
    {reply, ok, #state{auctions = #{}}};

handle_call({create_auction, AuctionReq}, _From, State = #state{auctions = Auctions}) ->
    Id = maps:get(<<"id">>, AuctionReq),
    case maps:is_key(Id, Auctions) of
        true ->
            {reply, {error, exists}, State};
        false ->
            %% Store auction. In a real app, we would use english_auction:new/1 etc.
            %% Here we just store the map for simplicity, or wrap it.
            %% The Haskell spec expects specific fields.
            Auction = AuctionReq#{
                <<"bids">> => [],
                <<"winner">> => null,
                <<"winnerPrice">> => null,
                <<"type">> => <<"English|0|0|0">> %% Default for test
            },
            NewAuctions = maps:put(Id, Auction, Auctions),
            {reply, {ok, Auction}, State#state{auctions = NewAuctions}}
    end;

handle_call({add_bid, AuctionId, BidReq, User}, _From, State = #state{auctions = Auctions}) ->
    case maps:find(AuctionId, Auctions) of
        {ok, Auction} ->
            Amount = maps:get(<<"amount">>, BidReq),
            %% Logic to add bid.
            %% For now, just append.
            Bids = maps:get(<<"bids">>, Auction),
            NewBid = #{
                <<"amount">> => Amount,
                <<"bidder">> => User,
                <<"auction">> => AuctionId,
                <<"user">> => User,
                <<"at">> => <<"2018-08-04T00:00:00Z">> %% Mock time
            },
            NewBids = [NewBid | Bids], %% Prepend
            NewAuction = Auction#{<<"bids">> => NewBids},
            NewAuctions = maps:put(AuctionId, NewAuction, Auctions),
            {reply, {ok, NewBid}, State#state{auctions = NewAuctions}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_auctions, _From, State = #state{auctions = Auctions}) ->
    {reply, maps:values(Auctions), State};

handle_call({get_auction, Id}, _From, State = #state{auctions = Auctions}) ->
    case maps:find(Id, Auctions) of
        {ok, Auction} -> {reply, {ok, Auction}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% Helpers

encode_auction_added(Auction) ->
    %% "$type" .= String "AuctionAdded", "at" .= String "2018-08-04T00:00:00Z", "auction" ...
    Event = #{
        <<"$type">> => <<"AuctionAdded">>,
        <<"at">> => <<"2018-08-04T00:00:00Z">>,
        <<"auction">> => format_auction_event(Auction)
    },
    json:encode(Event).

encode_bid_accepted(Bid) ->
    %% "$type" .= String "BidAccepted", "at" ... "bid" ...
    Event = #{
        <<"$type">> => <<"BidAccepted">>,
        <<"at">> => <<"2018-08-04T00:00:00Z">>,
        <<"bid">> => Bid
    },
    json:encode(Event).

format_auction(Auction) ->
    %% Format for GET /auctions/:id
    %% "currency", "expiry", "id", "startsAt", "title", "bids", "winner", "winnerPrice"
    %% Ensure keys are correct.
    Auction.

format_auction_event(Auction) ->
    %% Format for AuctionAdded event
    %% "id", "startsAt", "title", "expiry", "user", "type", "currency"
    %% We need to construct "user" and "type" if missing.
    Auction#{
        <<"user">> => <<"BuyerOrSeller|a1|Test">>, %% Mock
        <<"expiry">> => maps:get(<<"endsAt">>, Auction) %% Map endsAt to expiry
    }.
