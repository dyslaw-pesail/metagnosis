-module(mg).

%%% API functions
-export([coords_to_list/1,
         lists_to_atom/1]).

coords_to_list({X, Y, Z}) ->
    lists:concat([erlang:integer_to_list(X), ".",
                 erlang:integer_to_list(Y), ".",
                 erlang:integer_to_list(Z)]).

lists_to_atom(Lists) ->
    erlang:list_to_atom(lists:concat(Lists)).

