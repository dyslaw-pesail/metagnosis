-module(mg_world_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(WorldName) ->
    supervisor:start_link({local, mg:lists_to_atom([WorldName, "-world_sup"])}, ?MODULE, [WorldName]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([WorldName]) ->
    ClusterSup = { mg_cluster_sup, { mg_cluster_sup, start_link, [WorldName] },
                   permanent, infinity, supervisor, [mg_cluster_sup] },
    SimulationSup = { mg_simulation_sup, { mg_simulation_sup, start_link, [WorldName] },
                      permanent, infinity, supervisor, [mg_simulation_sup] },
    Children = [ClusterSup, SimulationSup],
    RestartStrategy = {one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
