-module(mg_services_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0,
         start_world_sup/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-include("defines/world.hrl").
-define(WORLD_SUP_SPEC(WorldName), 
        {?WORLD_SUP_NAME(WorldName),
         {mg_world_sup, start_link, [WorldName]}, 
         permanent, infinity, supervisor, [mg_world_sup]}).
start_world_sup(WorldName) ->
    supervisor:start_child(?MODULE, ?WORLD_SUP_SPEC(WorldName)). 

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    Db = {mg_db_sup, {mg_db_sup, start_link, []}, permanent, infinity, supervisor, [mg_db_sup]},
    RestartStrategy = {one_for_one, 0, 1}, 
    {ok, {RestartStrategy, [Db]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
