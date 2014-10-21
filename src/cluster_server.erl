-module(cluster_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([get_voxel/2, set_voxel/3]).
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

get_voxel([AddrComp|Tail], Block) -> 
    case (Block#block.attributes)#attributes.fill of
            empty -> Block#block{
                        c000 = empty, c001 = empty,
                        c010 = empty, c011 = empty,
                        c100 = empty, c101 = empty,
                        c110 = empty, c111 = empty
                     };
            full -> Block#block{
                        c000 = full, c001 = full,
                        c010 = full, c011 = full,
                        c100 = full, c101 = full,
                        c110 = full, c111 = full
                    };
            partial -> get_voxel(Tail, 
                case AddrComp of 
                    c000 -> Block#block.c000;
                    c001 -> Block#block.c001;
                    c010 -> Block#block.c010;
                    c011 -> Block#block.c011;
                    c100 -> Block#block.c100;
                    c101 -> Block#block.c101;
                    c110 -> Block#block.c110;
                    c111 -> Block#block.c111;
                    _ -> throw(unimplemented)
                end)
    end;                       
get_voxel([], Block) -> Block.

set_voxel(Addr, B, S) ->
    {_, V} = validate_voxel(substitute_voxel(Addr, B, S)),
    V.

substitute_voxel([A|T], B, S) when B =:= empty orelse B =:= full ->
    case A of
        c000 -> #block{c000 = substitute_voxel(T, B, S), c001 = B, c010 = B, c011 = B, c100 = B, c101 = B, c110 = B, c111 = B};
        c001 -> #block{c001 = substitute_voxel(T, B, S), c000 = B, c010 = B, c011 = B, c100 = B, c101 = B, c110 = B, c111 = B};
        c010 -> #block{c010 = substitute_voxel(T, B, S), c000 = B, c001 = B, c011 = B, c100 = B, c101 = B, c110 = B, c111 = B};
        c011 -> #block{c011 = substitute_voxel(T, B, S), c000 = B, c001 = B, c010 = B, c100 = B, c101 = B, c110 = B, c111 = B};
        c100 -> #block{c100 = substitute_voxel(T, B, S), c000 = B, c001 = B, c010 = B, c011 = B, c101 = B, c110 = B, c111 = B};
        c101 -> #block{c101 = substitute_voxel(T, B, S), c000 = B, c001 = B, c010 = B, c011 = B, c100 = B, c110 = B, c111 = B};
        c110 -> #block{c110 = substitute_voxel(T, B, S), c000 = B, c001 = B, c010 = B, c011 = B, c100 = B, c101 = B, c111 = B};
        c111 -> #block{c111 = substitute_voxel(T, B, S), c000 = B, c001 = B, c010 = B, c011 = B, c100 = B, c101 = B, c110 = B}
    end;
substitute_voxel([A|T], B, S) -> 
    case A of
        c000 -> B#block{c000 = substitute_voxel(T, B#block.c000, S)};
        c001 -> B#block{c001 = substitute_voxel(T, B#block.c001, S)};
        c010 -> B#block{c010 = substitute_voxel(T, B#block.c010, S)};
        c011 -> B#block{c011 = substitute_voxel(T, B#block.c011, S)};
        c100 -> B#block{c100 = substitute_voxel(T, B#block.c100, S)};
        c101 -> B#block{c101 = substitute_voxel(T, B#block.c101, S)};
        c110 -> B#block{c110 = substitute_voxel(T, B#block.c110, S)};
        c111 -> B#block{c111 = substitute_voxel(T, B#block.c111, S)}
    end;
substitute_voxel([], _, S) -> S.

validate_voxel(empty) -> {0, empty };
validate_voxel(full) -> {2, full };
validate_voxel(S) when is_record(S, block) -> 
    L = [validate_voxel(S#block.c000),
         validate_voxel(S#block.c001),
         validate_voxel(S#block.c010),
         validate_voxel(S#block.c011),
         validate_voxel(S#block.c100),
         validate_voxel(S#block.c101),
         validate_voxel(S#block.c110),
         validate_voxel(S#block.c111)],
    LC = lists:foldl(fun({X,_}, A) -> X + A end, 0, L),
    NLC = if LC =:= 16 -> 2;
             LC < 16 andalso LC > 0 -> 1;
             LC =:= 0 -> 0
          end,
    { NLC,
      S#block{c000 = element(2, lists:nth(1, L)),
            c001 = element(2, lists:nth(2, L)),
            c010 = element(2, lists:nth(3, L)),
            c011 = element(2, lists:nth(4, L)),
            c100 = element(2, lists:nth(5, L)),
            c101 = element(2, lists:nth(6, L)),
            c110 = element(2, lists:nth(7, L)),
            c111 = element(2, lists:nth(8, L)),
            attributes = (S#block.attributes)#attributes{fill = 
                if LC =:= 16 -> full;
                   LC < 16 andalso LC > 0 -> partial;
                   LC =:= 0 -> empty
                end
            }
           }}.
