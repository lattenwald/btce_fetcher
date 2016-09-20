-module(btce_fetcher_store).
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([start/0, start_link/0]).

-include("btce_fetcher.hrl").

-record(state, {data}).

start() ->
    gen_server:start({local, btce_store}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, btce_store}, ?MODULE, [], []).

init([]) ->
    {ok, #state{data=#{}}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast({store, Item}, State) ->
    {noreply, store(State, Item)};
%% XXX this is storage-dependent, make sure it's adequate to current storage type
handle_cast(print, State) ->
    io:format("state: ~p~n", [State]),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.


handle_call({store, Item}, _From, State) ->
    {reply, ok, store(State, Item)};
handle_call(_Request, _From, State) ->
    {reply, what, State}.

%% TODO move to ets, then to mnesia
store(Storage=#state{data=Data}, Items) ->
    NewData = lists:foldl(fun(Item=#transaction{tid=Tid}, Acc) ->
                                  maps:put(Tid, Item, Acc) end,
                          Data,
                          Items),
    NewStorage = Storage#state{data=NewData},
    NewStorage.

%% TODO will need some way to examine storage
