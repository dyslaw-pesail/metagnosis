-module(mg).

%%% API functions
-export([coords_to_list/1,
         get_cluster_server_atom/2, 
         get_cluster_server_pid/2,
         lists_to_atom/1]).

coords_to_list({X, Y, Z}) ->
    lists:concat([erlang:integer_to_list(X), ".",
                 erlang:integer_to_list(Y), ".",
                 erlang:integer_to_list(Z)]).

get_cluster_server_atom(WorldName, Coords) ->
    erlang:list_to_atom(lists:concat([WorldName, "-", coords_to_list(Coords)])).

get_cluster_server_pid(WorldName, Coords) ->
    whereis(get_cluster_server_atom(WorldName, Coords)).

lists_to_atom(Lists) ->
    erlang:list_to_atom(lists:concat(Lists)).

