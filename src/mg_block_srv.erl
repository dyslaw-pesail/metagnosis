-module(mg_block_srv).

-behaviour(gen_server).

%% API functions
-export([clear_dirty/1,
         get_block/2,
         identify_self/1,
         set_block/3,
         start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("defines/block.hrl").
-include("records/block.hrl").
-record(state, {block, coords, ets_world_name, world_name}).

%%%===================================================================
%%% API functions
%%%===================================================================

clear_dirty(Pid) ->
    gen_server:call(Pid, clear_dirty).

get_block(Pid, Addr) ->
    gen_server:call(Pid, {get_block, Addr}).

identify_self(Pid) ->
    gen_server:call(Pid, identify_self).

set_block(Pid, Addr, Block) ->
    gen_server:call(Pid, {set_block, Addr, Block}).

start_link(WorldName, ClusterCoords) ->
    gen_server:start_link(?MODULE, [WorldName, ClusterCoords], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([WorldName, BlockCoords]) ->
    process_flag(trap_exit, true), % to catch EXIT signal and delete pid from ETS table
    Block = initialize(WorldName, BlockCoords),
    EtsWorldName = ?ETS_BLOCK_TO_PID_NAME(WorldName),
    ets:insert(EtsWorldName, {BlockCoords, self()}),
    {ok, #state{block = Block, coords = blockCoords, 
                ets_world_name = EtsWorldName, world_name = WorldName}}.

handle_call(clear_dirty, _From, State) ->
    {reply, ok, State#state{block = clear_dirty_block_impl(State#state.block)}};
handle_call({get_block, Addr}, _From, State) ->
    {reply, get_block_impl(Addr, State#state.block), State};
handle_call(identify_self, _From, State) ->
    {reply, {State#state.world_name, State#state.coords}, State};
handle_call({set_block, Addr, Block}, _From, State) ->
    {reply, ok, State#state{block = set_block_impl(Addr, State#state.block, Block)}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(State#state.ets_world_name, State#state.coords),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

initialize(WorldName, BlockCoords) ->
    #block{}. % Temporary; it will load cluster data from persistence

-define(GET_BLOCK_MACRO(Addr), 
get_block_impl([Addr | Tl], B) -> 
    case (B#block.attr)#block_attr.fill of
        empty -> empty;
        nonuniform -> get_block_impl(Tl, B#block.Addr) 
    end).

?GET_BLOCK_MACRO(c000);
?GET_BLOCK_MACRO(c001);
?GET_BLOCK_MACRO(c010);
?GET_BLOCK_MACRO(c011);
?GET_BLOCK_MACRO(c100);
?GET_BLOCK_MACRO(c101);
?GET_BLOCK_MACRO(c110);
?GET_BLOCK_MACRO(c111);
get_block_impl([], B) -> B.

-define(SET_BLOCK_L_MACRO(Addr),
set_block_impl([Addr | Tl], B, S) ->
    case B of
        empty -> #block{Addr = set_block_impl(Tl, empty, S), 
                        attr = #block_attr{dirty = true, fill = nonuniform}};
        _ -> B#block{Addr = set_block_impl(Tl, B#block.Addr, S),
                     attr = (B#block.attr)#block_attr{dirty = true}} 
    end).

check_for_empty_subblocks(B) ->
    if  B#block.c000 == empty andalso
        B#block.c001 == empty andalso
        B#block.c010 == empty andalso
        B#block.c011 == empty andalso
        B#block.c100 == empty andalso
        B#block.c101 == empty andalso
        B#block.c110 == empty andalso
        B#block.c111 == empty -> empty;
        true -> nonuniform end.

-define(SET_BLOCK_S_MACRO(Addr),
set_block_impl([Addr], B, S) -> 
    case S of
        empty -> B#block{Addr = S,
                         attr = #block_attr{dirty = true, fill = check_for_empty_subblocks(B)}};
        _ -> B#block{Addr = S,
                     attr = #block_attr{dirty = true, fill = nonuniform}}
    end).
            
?SET_BLOCK_S_MACRO(c000);
?SET_BLOCK_S_MACRO(c001);
?SET_BLOCK_S_MACRO(c010);
?SET_BLOCK_S_MACRO(c011);
?SET_BLOCK_S_MACRO(c100);
?SET_BLOCK_S_MACRO(c101);
?SET_BLOCK_S_MACRO(c110);
?SET_BLOCK_S_MACRO(c111);    
?SET_BLOCK_L_MACRO(c000);
?SET_BLOCK_L_MACRO(c001);
?SET_BLOCK_L_MACRO(c010);
?SET_BLOCK_L_MACRO(c011);
?SET_BLOCK_L_MACRO(c100);
?SET_BLOCK_L_MACRO(c101);
?SET_BLOCK_L_MACRO(c110);
?SET_BLOCK_L_MACRO(c111).

-define(CLEAR_DIRTY_SUBBLOCK(Addr), Addr = clear_dirty_block_impl(B#block.Addr)).
clear_dirty_block_impl(empty) -> empty;
clear_dirty_block_impl(B) ->
    B#block{?CLEAR_DIRTY_SUBBLOCK(c000),
            ?CLEAR_DIRTY_SUBBLOCK(c001),
            ?CLEAR_DIRTY_SUBBLOCK(c010),
            ?CLEAR_DIRTY_SUBBLOCK(c011),
            ?CLEAR_DIRTY_SUBBLOCK(c100),
            ?CLEAR_DIRTY_SUBBLOCK(c101),
            ?CLEAR_DIRTY_SUBBLOCK(c110),
            ?CLEAR_DIRTY_SUBBLOCK(c111),
            attr = (B#block.attr)#block_attr{dirty = false}}.
