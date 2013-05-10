%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 16 Feb 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('clr').
-author('mats cronqvist').
-export([go/0]).
-export([octree_new/1
         ,octree_put/3
         ,octree_get/2
         ,octree_clear/2
         ,octree_first/1
         ,octree_last/1
         ,octree_next/2
         ,octree_prev/2]).

go() ->
  mk_table(),
  fill_table().

mk_table() ->
  spawn(fun()->
            try 
              register(?MODULE,self()),
              ets:new(?MODULE,[named_table,ordered_set,public]),
              receive _ -> ok end
            catch
              _:_ -> ok
            end
        end).

fill_table() -> fill_table("/usr/X11/share/X11/rgb.txt").
fill_table(F) ->
  parse(binary_to_list(element(2,file:read_file(F)))).

parse(Str) -> lists:reverse(parse(Str,ws,[])).

-define(is_ws(C), (C==$  orelse C==$\t orelse C==$\n orelse C==$\r)).
-define(is_digit(C), ($0 =< C andalso C =< $9)).
-define(is_str(S), is_integer(hd(S))).
parse([X|R],ws,O) when ?is_ws(X)                  -> parse(R,ws,O);
parse([X|R],ws,O) when ?is_digit(X)               -> parse(R,ii,[[X]|O]);
parse([X|R],ws,O)                                 -> parse(R,wd,[[X]|O]);
parse([X|R],ii,[P|O]) when ?is_ws(X)              -> parse(R,ws,[i(r(P))|O]);
parse([X|R],ii,[P|O]) when ?is_digit(X)           -> parse(R,ii,[[X|P]|O]);
parse([X|R],ii,[P|O])                             -> parse(R,wd,[[X|P]|O]);
parse([X|R],wd,[P,Q|O]) when ?is_ws(X),?is_str(Q) -> parse(R,ws,[j(r(P),Q)|O]);
parse([X|R],wd,[P|O]) when ?is_ws(X)              -> parse(R,ws,[r(P)|O]);
parse([X|R],wd,[P|O])                             -> parse(R,wd,[[X|P]|O]);
parse([],ii,[P|O])                                -> [i(r(P))|O];
parse([],_,O)                                     -> O.

i(S) -> list_to_integer(S).
r(S) -> lists:reverse(S).
j(P,Q) -> string:join([Q,P]," ").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% octal tree

%% make sure that key K does not exist
octree_clear([],Tree) ->
  Tree;
octree_clear([I|K],Tree) ->
  case element(I,Tree) of
    {}      -> Tree;
    {K,_}   -> sete(I,Tree,{});
    {_,_}   -> Tree;
    SubTree -> sete(I,Tree,octree_clear(K,SubTree))
  end.

%% store value V associated with key K.
%% K is a list of integers between 1 and N
%% N is the cardinality of the tree
octree_put([],V,_) ->
  %% a final leaf
  {[],V};
octree_put([I|K],V,Tree) ->
  %% sub_tree can be a non-final leaf
  case element(I,Tree) of
    {}      -> sete(I,Tree,{K,V});
    {K,_}   -> sete(I,Tree,{K,V});
    {KO,VO} -> sete(I,Tree,octree_put(K,V,octree_put(KO,VO,octree_new())));
    SubTree -> sete(I,Tree,octree_put(K,V,SubTree))
  end.

sete(I,T,V) -> setelement(I,T,V).

%% create an empty  tree with cardinality N (defaults to 8)
octree_new() -> octree_new(8).
octree_new(C) -> erlang:make_tuple(C,{}).

%% return value V associated with kwy K
%% throw error if K does not exist
octree_get([],_) ->
  error(no_such_key);
octree_get([I|K],Tree) ->
  case element(I,Tree) of
    {}      -> error(no_such_key);
    {K,V}   -> V;
    {_,_}   -> error(no_such_key);
    SubTree -> octree_get(K,SubTree)
  end.

%% return first Key in Tree
octree_first(Tree) ->
  edge(1,1,Tree).

%% return last Key in Tree
octree_last(Tree) ->
  edge(tuple_size(Tree),-1,Tree).

edge(I0,Inc,Tree) ->
  case next_subtree(I0,Inc,Tree) of
    {key,K} -> K;
    {I,SubTree}  -> [I|edge(I0,Inc,SubTree)];
    {} -> []
  end.

next_subtree(I,Inc,Tree) -> 
  try element(I,Tree) of
    {} -> next_subtree(I+Inc,Inc,Tree);
    {K,_} -> {key,[I|K]};
    Subtree -> {I,Subtree}
  catch 
    _:_ -> {}
  end.

octree_prev(_,_) ->ok.

octree_next(Key,Tree) ->
  
