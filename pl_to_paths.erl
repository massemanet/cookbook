% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 17 Sep 2014 by  <masse@klarna.com>

%% @doc
%% @end

-module('pl_to_paths').
-export([proplist_to_paths/1,paths_to_proplist/1]).

proplist_to_paths(PL) ->
  pl2ps(PL,[]).

pl2ps([],_)             -> [];
pl2ps([{K,V}|T],Prefix) -> pl2ps(V,Prefix++[K])++pl2ps(T,Prefix);
pl2ps(V,Prefix)         -> [{Prefix,V}].

paths_to_proplist(Paths) ->
  element(3,ps2pl({Paths,[],[]})).

ps2pl({[],Prefix,Acc}) -> {[],Prefix,Acc};
ps2pl({[{Path,V}|Paths],Prefix,Acc}) ->
  case strip(Prefix,Path) of
    nil  -> {[{Path,V}|Paths],tl(Prefix),Acc};
    [K]  -> ps2pl({Paths,Prefix,[{K,V}|Acc]});
    [T|_]-> {NPaths,NPrefix,NAcc} = ps2pl({[{Path,V}|Paths],Prefix++[T],Acc}),
            ps2pl({NPaths,NPrefix,[{T,NAcc}]})
  end.

strip([X|Pre],[X|Whole]) -> strip(Pre,Whole);
strip([],Whole)          -> Whole;
strip(_,_)               -> nil.
