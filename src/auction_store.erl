-module(auction_store).
-behaviour(gen_server).

-export([start_link/0, create_auction/3, get_auctions/0, get_auction/1, add_bid/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DETS_TABLE, auction_store_dets).

-record(auction_entry, {
    info :: map(),
    logic_mod :: atom(),
    logic_state :: term()
}).

-record(state, {auctions = #{} :: #{integer() => #auction_entry{}}}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_auction(Info, Mod, LogicState) ->
    gen_server:call(?SERVER, {create_auction, Info, Mod, LogicState}).

add_bid(AuctionId, Bid) ->
    gen_server:call(?SERVER, {add_bid, AuctionId, Bid}).

get_auctions() ->
    gen_server:call(?SERVER, get_auctions).

get_auction(Id) ->
    gen_server:call(?SERVER, {get_auction, Id}).

init([]) ->
    DetsFile = application:get_env(auctions, dets_file, "auction_store.dets"),
    {ok, ?DETS_TABLE} = dets:open_file(?DETS_TABLE, [{file, DetsFile}, {type, set}]),
    Auctions = dets:foldl(fun({Id, Entry}, Acc) -> maps:put(Id, Entry, Acc) end, #{}, ?DETS_TABLE),
    {ok, #state{auctions = Auctions}}.

handle_call({create_auction, Info, Mod, LogicState}, _From, State = #state{auctions = Auctions}) ->
    Id = maps:get(id, Info),
    Entry = #auction_entry{info = Info, logic_mod = Mod, logic_state = LogicState},
    ok = dets:insert(?DETS_TABLE, {Id, Entry}),
    NewAuctions = maps:put(Id, Entry, Auctions),
    {reply, ok, State#state{auctions = NewAuctions}};
handle_call({add_bid, AuctionId, Bid}, _From, State = #state{auctions = Auctions}) ->
    case maps:find(AuctionId, Auctions) of
        {ok, Entry = #auction_entry{logic_mod = Mod, logic_state = LogicState}} ->
            Now = os:system_time(millisecond),
            LogicState1 = Mod:inc(Now, LogicState),
            case Mod:add_bid(Bid, LogicState1) of
                {LogicState2, ok} ->
                    NewEntry = Entry#auction_entry{logic_state = LogicState2},
                    ok = dets:insert(?DETS_TABLE, {AuctionId, NewEntry}),
                    NewAuctions = maps:put(AuctionId, NewEntry, Auctions),
                    {reply, ok, State#state{auctions = NewAuctions}};
                {LogicState2, {error, Reason}} ->
                    NewEntry = Entry#auction_entry{logic_state = LogicState2},
                    ok = dets:insert(?DETS_TABLE, {AuctionId, NewEntry}),
                    NewAuctions = maps:put(AuctionId, NewEntry, Auctions),
                    {reply, {error, Reason}, State#state{auctions = NewAuctions}}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call(get_auctions, _From, State = #state{auctions = Auctions}) ->
    Result = [format_auction(Entry) || Entry <- maps:values(Auctions)],
    {reply, Result, State};
handle_call({get_auction, Id}, _From, State = #state{auctions = Auctions}) ->
    Reply =
        case Auctions of
       #{Id := Entry} ->
           format_auction(Entry);
       #{} ->
           not_found
   end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(?DETS_TABLE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_auction(#auction_entry{info = Info, logic_mod = Mod, logic_state = LogicState}) ->
    %% Reconstruct the auction map for the API
    %% We need to extract bids and winner from LogicState
    Bids = Mod:get_bids(LogicState),
    WinnerInfo =
        case Mod:try_get_amount_and_winner(LogicState) of
            {ok, {Amount, Winner}} -> #{currentBid => Amount, winner => Winner};
            undefined -> #{}
        end,

    %% Convert Info keys back to binaries if needed?
    %% The API handlers seem to expect maps that can be encoded to JSON.
    %% json:encode works with atom keys too.
    %% But we need to match the AuctionModel structure.

    Base = #{
        <<"id">> => maps:get(id, Info),
        <<"title">> => maps:get(title, Info),
        <<"startsAt">> => maps:get(starts_at, Info),
        <<"endsAt">> => maps:get(ends_at, Info),
        <<"currency">> => maps:get(currency, Info),
        <<"type">> => auction_serialization:encode_auction_type(maps:get(type, Info)),
        <<"bids">> => [format_bid(B) || B <- Bids]
    },
    maps:merge(Base, WinnerInfo).

format_bid(Bid) ->
    %% Bid is #{amount => ..., user => ..., time => ...}
    %% API expects BidModel: amount, bidder, at
    #{
        <<"amount">> => maps:get(amount, Bid),
        <<"bidder">> => maps:get(user, Bid),
        <<"at">> => maps:get(time, Bid)
    }.
