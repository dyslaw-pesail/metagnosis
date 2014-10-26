-module(mg_db_pull_worker).

-behaviour(gen_server).

-export([pull_world_block/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         start_link/1]).

-include("defines/db.hrl").
-include("errors/db.hrl").
-record(state, {ref}).

pull_world_block(Server, WorldName, BlockCoords) ->
    gen_server:cast(Server, {pull_world_block, WorldName, BlockCoords, self()}).

start_link(Ref) ->
    gen_server:start_link(?MODULE, [Ref], []).

init([Ref]) ->
    {ok, #state{ref=Ref}}.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast({pull_world_block, WorldName, BlockCoords, ClientPid}, State) ->
    pull_world_block_async(WorldName, BlockCoords, {ClientPid, State#state.ref}),
    {stop, normal, State};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%==================================================
%%% Internal functions
%%%==================================================

pull_world_block_async(WorldName, BlockCoords, {ClientPid, RequestRef}) ->
    DbName = ?WORLD_BLOCK_DB_NAME(WorldName),
    ClientPid ! case mnesia:read(DbName, BlockCoords) of
                    [] -> ?ERROR_DB_PULL_NOT_FOUND;
                    [Result] -> ?DB_PULL_RESULT(Result, RequestRef)
                end.
