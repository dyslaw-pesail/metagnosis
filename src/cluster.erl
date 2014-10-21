-module(cluster).
-behaviour(gen_server).

-export([start_link/0, get_voxel/2, set_voxel/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_voxel(Cluster, Addr) ->
    gen_server:call(?MODULE, {get_voxel, Cluster, Addr}).
set_voxel(Cluster, Addr, Voxel) ->
    gen_server:call(?MODULE, {set_voxel, Cluster, Addr, Voxel}).

init([]) ->
    {ok, #state{}}.

handle_call({get_voxel, {X, Y, Z}, Addr}, _From, State) ->
    Name = cluster_to_atom(X, Y, Z),
    start_cluster_server(Name),
    {reply, gen_server:call(Name, {get_voxel, Addr}), State};
handle_call({set_voxel, {X, Y, Z}, Addr, Voxel}, _From, State) ->
    Name = cluster_to_atom(X, Y, Z),
    start_cluster_server(Name),
    {reply, gen_server:call(Name, {set_voxel, Addr, Voxel}), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_cluster_server(Name) ->
    case erlang:whereis(Name) of
        undefined -> cluster_server:start_link(Name), io:format("Started cluster: ~s~n", [Name]);
        _ -> ok
    end.
cluster_to_atom(X, Y, Z) ->
    erlang:list_to_atom(lists:concat([erlang:integer_to_list(X), 
                                      ".", 
                                      erlang:integer_to_list(Y), 
                                      ".", 
                                      erlang:integer_to_list(Z)])).

