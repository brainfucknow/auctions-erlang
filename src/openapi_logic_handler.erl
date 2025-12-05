-module(openapi_logic_handler).

-include_lib("kernel/include/logger.hrl").

-type accept_callback_return() ::
        stop
        | boolean()
        | {true, iodata()}
        | {created, iodata()}
        | {see_other, iodata()}.
-type provide_callback_return() ::
        stop
        | cowboy_req:resp_body().
-type api_key_callback() ::
    fun((openapi_api:operation_id(), binary()) -> {true, context()} | {false, iodata()}).
-type accept_callback() ::
    fun((openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
            {accept_callback_return(), cowboy_req:req(), context()}).
-type provide_callback() ::
    fun((openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
            {cowboy_req:resp_body(), cowboy_req:req(), context()}).
-type context() :: #{_ := _}.

-export_type([context/0, api_key_callback/0,
              accept_callback_return/0, provide_callback_return/0,
              accept_callback/0, provide_callback/0]).

-optional_callbacks([api_key_callback/2]).

-callback api_key_callback(openapi_api:operation_id(), binary()) ->
    {true, context()} | {false, iodata()}.

-callback accept_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {accept_callback_return(), cowboy_req:req(), context()}.

-callback provide_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {provide_callback_return(), cowboy_req:req(), context()}.

-export([api_key_callback/2, accept_callback/4, provide_callback/4]).
-ignore_xref([api_key_callback/2, accept_callback/4, provide_callback/4]).

-spec api_key_callback(openapi_api:operation_id(), binary()) -> {true, #{}}.
api_key_callback(OperationID, ApiKey) ->
    ?LOG_ERROR(#{what => "Got not implemented api_key_callback request",
                 operation_id => OperationID,
                 api_key => ApiKey}),
    {true, #{}}.

-spec accept_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {accept_callback_return(), cowboy_req:req(), context()}.
accept_callback('createAuction', 'create_auction', Req, Context) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    try json:decode(Body) of
        Auction ->
            auction_store:create_auction(Auction),
            ?LOG_INFO(#{what => "Auction created", auction => Auction}),
            Req2 = cowboy_req:set_resp_body(Body, Req1),
            {true, Req2, Context}
    catch
        _:_ ->
            {false, Req1, Context}
    end;
accept_callback(Class, OperationID, Req, Context) ->
    ?LOG_ERROR(#{what => "Got not implemented request to process",
                 class => Class,
                 operation_id => OperationID,
                 request => Req,
                 context => Context}),
    {false, Req, Context}.

-spec provide_callback(openapi_api:class(), openapi_api:operation_id(), cowboy_req:req(), context()) ->
    {cowboy_req:resp_body(), cowboy_req:req(), context()}.
provide_callback('getAuctions', 'get_auctions', Req, Context) ->
    Auctions = auction_store:get_auctions(),
    Body = json:encode(Auctions),
    {Body, Req, Context};
provide_callback('getAuctions', 'get_auction', Req, Context) ->
    AuctionIdBin = cowboy_req:binding(auctionId, Req),
    try binary_to_integer(AuctionIdBin) of
        AuctionId ->
            case auction_store:get_auction(AuctionId) of
                not_found ->
                    {json:encode(null), Req, Context};
                Auction ->
                    {json:encode(Auction), Req, Context}
            end
    catch
        _:_ ->
            {json:encode(null), Req, Context}
    end;
provide_callback(Class, OperationID, Req, Context) ->
    ?LOG_ERROR(#{what => "Got not implemented request to process",
                 class => Class,
                 operation_id => OperationID,
                 request => Req,
                 context => Context}),
    {<<>>, Req, Context}.
