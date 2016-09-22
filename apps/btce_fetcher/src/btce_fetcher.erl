-module(btce_fetcher).
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([start/2, start_link/2]).

-define(URL, "https://btc-e.com/api/3/trades/btc_usd?limit=~B").

-record(state, {url, storage, timeout}).
-include("btce_fetcher.hrl").

%%% API
start(Timeout, FetchLimit) ->
    gen_server:start(?MODULE, [Timeout, FetchLimit], []).

start_link(Timeout, FetchLimit) ->
    gen_server:start_link(?MODULE, [Timeout, FetchLimit], []).


%%% callbacks start/stop/change
init([Timeout, FetchLimit]) ->
    Url = io_lib:format(?URL, [FetchLimit]),
    ok = inets:start(),
    ok = ssl:start(),
    self() ! run,
    {ok, #state{url=Url, storage=?STORAGE, timeout=Timeout}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% callbacks call/cast (not used)
handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

%%% callback info (used)
handle_info(run, State=#state{url=Url, timeout=Timeout, storage=Storage}) ->
    Data = fetch_data(Url),
    timer:send_after(Timeout, run),
    store_data(Data, Storage),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%% helpers
type_to_atom(<<"bid">>) -> bid;
type_to_atom(<<"ask">>) -> ask.

fetch_data(Url) ->
    {ok, {_Status, _Headers, Body}} = httpc:request(Url),
    Data = jiffy:decode(Body, [return_maps]),
    Decoded = lists:map(
                fun(Item) ->
                        #transaction{timestamp=maps:get(<<"timestamp">>, Item),
                                     rate=maps:get(<<"price">>, Item),
                                     amount=maps:get(<<"amount">>, Item),
                                     type=type_to_atom(maps:get(<<"type">>, Item)),
                                     tid=maps:get(<<"tid">>, Item)}
                end, maps:get(<<"btc_usd">>, Data)),
    Decoded.

store_data(Data, Storage) ->
    gen_server:call(Storage, {store, Data}),
    ok.