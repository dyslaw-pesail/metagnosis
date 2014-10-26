-module(mg_db_sup).

-behaviour(supervisor).

%% API functions
-export([pull_world_block_async/2,
         pull_world_block_async/3,
         pull_world_block_sync/2,
         pull_world_block_sync/3,
         start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-include("errors/db.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

pull_world_block_async(WorldName, BlockCoords) ->
    pull_world_block_async(WorldName, BlockCoords, make_ref()).

pull_world_block_async(WorldName, BlockCoords, Ref) ->
    {ok, Pid} = supervisor:start_child(?MODULE, {
        make_ref(),
        {mg_db_pull_worker, start_link, [Ref]},
        transient, brutal_kill, worker, [mg_db_pull_worker]}),
    mg_db_pull_worker:pull_world_block(Pid, WorldName, BlockCoords).

pull_world_block_sync(WorldName, BlockCoords) ->
    pull_world_block_sync(WorldName, BlockCoords, 1000).

pull_world_block_sync(WorldName, BlockCoords, TimeOut) ->
    Ref = make_ref(),
    pull_world_block_async(WorldName, BlockCoords, Ref),
    receive
        {ok, Ref, Result} -> Result;
        Error -> Error
    after TimeOut -> ?ERROR_DB_TIMEOUT
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{one_for_one, 10, 1}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
