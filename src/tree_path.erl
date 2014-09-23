% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 17 Sep 2014 by  <masse@klarna.com>

%% @doc
%% converts a graph between tree form;
%% [{a,1},
%%  {b,[{ba,21},
%%      {bb,[{bba,221},
%%           {bbb,222}]}]},
%%  {c,3}]
%%
%% and path form;
%%
%% [{[c],3},
%%  {[b,bb,bbb],222},
%%  {[b,bb,bba],221},
%%  {[b,ba],21},
%%  {[a],1}]
%% @end

-module('tree_path').
-export([tree_to_path/1,path_to_tree/1]).

tree_to_path(Tree) ->
  t2p(Tree,[],[]).

t2p([],_,A)             -> A;
t2p([{K,V}|T],Prefix,A) -> t2p(T,Prefix,t2p(V,Prefix++[K],A));
t2p(V,Prefix,A)         -> [{Prefix,V}|A].

path_to_tree(Paths) ->
  {[],Acc} = p2t(lists:sort(Paths),[],[]),
  Acc.

p2t([],_,Acc) -> {[],Acc};
p2t([{Path,V}|_]=Paths,Prefix,Acc) ->
  case strip_prefix(Prefix,Path) of
    nil  -> {Paths,Acc};
    [K]  -> p2t(tl(Paths),Prefix,[{K,V}|Acc]);
    [T|_]-> {NPaths,NAcc} = p2t(Paths,Prefix++[T],[]),
            p2t(NPaths,Prefix,[{T,NAcc}|Acc])
  end.

strip_prefix([X|Pre],[X|Tail]) -> strip_prefix(Pre,Tail);
strip_prefix([],Tail)          -> Tail;
strip_prefix(_,_)              -> nil.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

t0_test() ->
  Tree = [{a,1},{b,[{ba,21},{bb,[{bba,221},{bbb,222}]}]},{c,3}],
  ?assertEqual(
     lists:sort(tree_to_path(path_to_tree(tree_to_path(Tree)))),
     lists:sort(tree_to_path(Tree))).

-endif.
