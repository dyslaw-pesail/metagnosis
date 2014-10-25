-module(mg_world_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include("defines/block.hrl").
-include("defines/world.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(WorldName) ->
    supervisor:start_link({local, ?WORLD_SUP_NAME(WorldName)}, ?MODULE, [WorldName]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([WorldName]) ->
    BlockSup = {?BLOCK_SUP_NAME(WorldName), {mg_block_sup, start_link, [WorldName]},
                permanent, infinity, supervisor, [mg_block_sup]},
    SimulationSup = {mg_simulation_sup, {mg_simulation_sup, start_link, [WorldName]},
                     permanent, infinity, supervisor, [mg_simulation_sup]},
    Children = [BlockSup, SimulationSup],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
