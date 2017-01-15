-module(btce_fetcher).
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([start/2, start_link/2]).

-define(URL, "https://btc-e.com/api/3/trades/btc_usd?limit=~B").
-define(RETRY_TIME, 10000).

-record(state, {url, timeout}).
-include("btce_fetcher.hrl").

%%% API
start(Timeout, FetchLimit) ->
    gen_server:start(?MODULE, [Timeout, FetchLimit], []).

start_link(Timeout, FetchLimit) ->
    gen_server:start_link(?MODULE, [Timeout, FetchLimit], []).


%%% callbacks start/stop/change
init([Timeout, FetchLimit]) ->
    Url = io_lib:format(?URL, [FetchLimit]),
    inets:start(),
    self() ! run,
    {ok, #state{url=Url, timeout=Timeout}}.

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
handle_info(run, State=#state{url=Url, timeout=Timeout}) ->
    Data = fetch_data(Url),
    {ok, _TRef} = timer:send_after(Timeout, run),
    store_data(Data),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%% helpers
-spec type_to_atom(binary()) -> transaction_type().
type_to_atom(<<"bid">>) -> bid;
type_to_atom(<<"ask">>) -> ask.

-spec fetch_data(string()) -> list(transaction()).
fetch_data(Url) ->
    Body = case httpc:request(Url) of
               {ok, {_Status, _Headers, Body1}} -> Body1;
               {connect_failed, Args} ->
                   timer:sleep(?RETRY_TIME),
                   error(connect_failed, Args);
               {error, socket_closed_remotely = Err} ->
                   timer:sleep(?RETRY_TIME),
                   error(Err)
           end,
    Data = try jiffy:decode(Body, [return_maps]) of
               Stuff -> Stuff
           catch
               {error, {ErrId, invalid_json}} ->
                   timer:sleep(?RETRY_TIME),
                   throw({error, {ErrId, invalid_json}})
           end,
    Decoded = lists:map(
                fun(Item) ->
                        #transaction{timestamp=maps:get(<<"timestamp">>, Item),
                                     rate=maps:get(<<"price">>, Item),
                                     amount=maps:get(<<"amount">>, Item),
                                     type=type_to_atom(maps:get(<<"type">>, Item)),
                                     tid=maps:get(<<"tid">>, Item)}
                end, maps:get(<<"btc_usd">>, Data)),
    Decoded.

store_data(Data) ->
    btce_fetcher_store:store_data(Data),
    ok.
