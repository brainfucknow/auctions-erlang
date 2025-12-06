-module(auctions_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = auctions_sup:start_link(),
    %% Start the HTTP server
    %% We use the default configuration as suggested in the README/generated code pattern
    %% but adapted for this app structure.
    {ok, _} = auctions_server:start(auctions_http,
        #{transport_opts => [{ip, {0,0,0,0}}, %% Listen on all interfaces
                             {port, 8080}
                            ]}),
    {ok, Pid}.

stop(_State) ->
    ok.
