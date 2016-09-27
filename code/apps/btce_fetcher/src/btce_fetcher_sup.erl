%%%-------------------------------------------------------------------
%% @doc btce_fetcher top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(btce_fetcher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    MaxRestart = 10, %% 10 restarts
    MaxTime = 60000, %% per minute
    {ok, Timeout} = application:get_env(timeout),
    {ok, FetchLimit} = application:get_env(fetch_limit),
    {ok, EnvDetsFile} = application:get_env(dets_file),
    DetsFile = case os:getenv("BTCE_FETCHER_STORAGE") of
                   false -> EnvDetsFile;
                   SomeFile -> SomeFile
               end,
    {ok, { {rest_for_one, MaxRestart, MaxTime},
           [{storage,
             {btce_fetcher_store, start_link, [DetsFile]},
             permanent,
             5000,
             worker,
             [btce_fetcher_store]
            },
            {fetcher,
             {btce_fetcher, start_link, [Timeout, FetchLimit]},
             permanent,
             5000,
             worker,
             [btce_fetcher]
            }]
         } }.

%%====================================================================
%% Internal functions
%%====================================================================
