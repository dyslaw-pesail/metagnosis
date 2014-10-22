-module(mg_services_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    WorldSup = {mg_world_sup, {mg_world_sup, start_link, ["entry"]},
                permanent, infinity, supervisor, [mg_world_sup]},
    Children = [WorldSup],
    RestartStrategy = {one_for_one, 0, 1}, 
    {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
