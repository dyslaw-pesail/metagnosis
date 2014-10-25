-module(mg_block_sup).

-behaviour(supervisor).

%% API functions
-export([get_server_pid/2,
         start_link/1,
         start_server/2]).

%% Supervisor callbacks
-export([init/1]).

-include("defines/block.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

get_server_pid(WorldName, BlockCoords) ->
    case ets:lookup(?ETS_BLOCK_TO_PID_NAME(WorldName), BlockCoords) of
        [] -> {ok, Pid} = start_server(WorldName, BlockCoords),
              Pid;
        [{BlockCoords, Pid}] when is_pid(Pid) -> Pid
    end.

start_link(WorldName) ->
    supervisor:start_link({local, ?BLOCK_SUP_NAME(WorldName)}, ?MODULE, [WorldName]).

-define(BLOCK_SRV_SPEC(WorldName, BlockCoords), 
        {?BLOCK_SRV_NAME(WorldName, BlockCoords),
         {mg_block_srv, start_link, [WorldName, BlockCoords]},
          permanent,
          5000,
          worker,
          [mg_block_srv]}).
start_server(WorldName, BlockCoords) ->
    supervisor:start_child(?BLOCK_SUP_NAME(WorldName), 
                           ?BLOCK_SRV_SPEC(WorldName, BlockCoords)).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([WorldName]) ->
    ets:new(?ETS_BLOCK_TO_PID_NAME(WorldName), [set, public, named_table, {keypos, 1}]),
    {ok, {{one_for_one, 0, 1}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
