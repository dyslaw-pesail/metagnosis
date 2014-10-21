-module(cluster_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([get_voxel/2]).
-include("records/block.hrl").
-record(state, {block = #block{}}).
-define(TIMEOUT, 10000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}, ?TIMEOUT}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    io:format('Timeout~n'),
    {noreply, State, ?TIMEOUT};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Accessors

select_subblock(c000, Block) -> Block#block.c000;
select_subblock(c001, Block) -> Block#block.c001;
select_subblock(c010, Block) -> Block#block.c010;
select_subblock(c011, Block) -> Block#block.c011;
select_subblock(c100, Block) -> Block#block.c100;
select_subblock(c101, Block) -> Block#block.c101;
select_subblock(c110, Block) -> Block#block.c110;
select_subblock(c111, Block) -> Block#block.c111.

get_voxel([AddrComp|Tail], Block) -> 
    case (Block#block.attributes)#attributes.fill of
            empty -> empty;
            full -> Block;
            partial -> get_voxel(Tail, 
                select_subblock(AddrComp, Block));
            _ -> throw(unimplemented)
    end;
get_voxel([], Block) -> Block.
