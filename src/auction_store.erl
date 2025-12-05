-module(auction_store).
-behaviour(gen_server).

-export([start_link/0, create_auction/1, get_auctions/0, get_auction/1, add_bid/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {auctions = #{}}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_auction(Auction) ->
    gen_server:call(?SERVER, {create_auction, Auction}).

add_bid(AuctionId, Bid) ->
    gen_server:call(?SERVER, {add_bid, AuctionId, Bid}).

get_auctions() ->
    gen_server:call(?SERVER, get_auctions).

get_auction(Id) ->
    gen_server:call(?SERVER, {get_auction, Id}).

init([]) ->
    {ok, #state{}}.

handle_call({create_auction, Auction}, _From, State = #state{auctions = Auctions}) ->
    Id = maps:get(<<"id">>, Auction),
    NewAuctions = maps:put(Id, Auction, Auctions),
    {reply, ok, State#state{auctions = NewAuctions}};

handle_call({add_bid, AuctionId, Bid}, _From, State = #state{auctions = Auctions}) ->
    case maps:find(AuctionId, Auctions) of
        {ok, Auction} ->
            Bids = maps:get(<<"bids">>, Auction, []),
            NewBids = [Bid | Bids],
            NewAuction = maps:put(<<"bids">>, NewBids, Auction),
            NewAuctions = maps:put(AuctionId, NewAuction, Auctions),
            {reply, ok, State#state{auctions = NewAuctions}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_auctions, _From, State = #state{auctions = Auctions}) ->
    {reply, maps:values(Auctions), State};

handle_call({get_auction, Id}, _From, State = #state{auctions = Auctions}) ->
    Reply = maps:get(Id, Auctions, not_found),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
