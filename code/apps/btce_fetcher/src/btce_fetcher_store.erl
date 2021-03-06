-module(btce_fetcher_store).
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([start/1, start_link/1]).
-export([store_data/1]).

-define(STORAGE, btce_store).
-define(STORAGE_REF, {global, ?STORAGE}).

-include("btce_fetcher.hrl").

-record(state, {table}).

start(DetsFile) ->
    gen_server:start(?STORAGE_REF, ?MODULE, [DetsFile], []).

start_link(DetsFile) ->
    gen_server:start_link(?STORAGE_REF, ?MODULE, [DetsFile], []).

init([DetsFile]) ->
    {ok, Ref} = dets:open_file(?STORAGE, [{keypos, #transaction.tid},
                                          {file, DetsFile}]),
    {ok, #state{table=Ref}}.

terminate(_Reason, #state{table=Table}) ->
    dets:close(Table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast({store, Item}, State) ->
    {noreply, store(State, Item)};
handle_cast(info, State=#state{table=Table}) ->
    io:format("dets:info ~p~n", [dets:info(Table)]),
    {noreply, State};
handle_cast(print, State=#state{table=Table}) ->
    dets:traverse(Table,
                  fun(Item) ->
                          io:format("~p~n", [Item]),
                          continue
                  end),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.


handle_call({store, Item}, _From, State) ->
    {reply, ok, store(State, Item)};
handle_call(info, _From, State=#state{table=Table}) ->
    {reply, {info, dets:info(Table)}, State};
handle_call(print, _From, State=#state{table=Table}) ->
    Data = dets:foldl(fun(Item, Acc) -> [Item|Acc] end, [], Table),
    {reply, {data, Data}, State};
handle_call(_Request, _From, State) ->
    {reply, what, State}.

-spec store_data([transaction()]) -> ok.
store_data(Items) ->
    gen_server:cast(?STORAGE_REF, {store, Items}),
    ok.

-spec store(#state{}, [transaction()]) -> #state{}.
store(Storage=#state{table=Table}, Items) ->
    ok = dets:insert(Table, Items),
    Storage.
