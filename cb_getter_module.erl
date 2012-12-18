%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  5 Jul 2012 by Mats Cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('cb_getter_module').
-author('Mats Cronqvist').
-export([all/0,new/1,get/1,set/2,dbg/0,dbg/1]).

all() -> [a,b,c].

get(a) -> "A";
get(b) -> "B";
get(c) -> "C";
get(_) -> exit(not_found).

set(K,V) ->
  new(?MODULE,[{K,V}|[kv(I)||I<-all()]]).

kv(K) -> {K,?MODULE:get(K)}.

new(Mod) ->
  new(Mod,[]).

new(Mod,All) ->
  code:load_binary(Mod,"",element(3,compile:forms(forms(Mod,All)))).

forms(Mod,All) ->
  lists:append([mk_attrs(Mod),mk_all_0(All),mk_get_1(All)],mk_boilerplate()).

mk_attrs(Mod) ->
  [{attribute,7,module,Mod},
   {attribute,9,export,[{all,0},{get,1},{set,2}]}].

mk_all_0(All) ->
  [{function,11,all,0,
    [{clause,11,[],[],
      [mk_all_args([K||{K,_}<-All])]}]}].

mk_all_args([H|T]) -> {cons,11,{atom,11,H},mk_all_args(T)};
mk_all_args([])    -> {nil,11}.

mk_get_1(A) ->
  [{function,13,get,1,
    [{clause,13,[{atom,13,K}],[],[{string,13,V}]}||{K,V}<-A]++
      [{clause,14,[{var,14,'_'}],[],
        [{call,14,{atom,14,exit},[{atom,14,not_found}]}]}]}].

mk_boilerplate() ->
{function,18,set,2,[{clause,18,[{var,18,'K'},{var,18,'V'}],[],[{call,19,{atom,19,new},[{atom,19,cb_getter_module},{cons,19,{tuple,19,[{var,19,'K'},{var,19,'V'}]},{lc,19,{call,19,{atom,19,kv},[{var,19,'I'}]},[{generate,19,{var,19,'I'},{call,19,{atom,19,all},[]}}]}}]}]}]},{function,21,kv,1,[{clause,21,[{var,21,'K'}],[],[{tuple,21,[{var,21,'K'},{call,21,{remote,21,{atom,21,cb_getter_module},{atom,21,get}},[{var,21,'K'}]}]}]}]},{function,23,new,1,[{clause,23,[{var,23,'Mod'}],[],[{call,24,{atom,24,new},[{var,24,'Mod'},{nil,24}]}]}]},{function,26,new,2,[{clause,26,[{var,26,'Mod'},{var,26,'All'}],[],[{call,27,{remote,27,{atom,27,code},{atom,27,load_binary}},[{var,27,'Mod'},{string,27,[]},{call,27,{atom,27,element},[{integer,27,3},{call,27,{remote,27,{atom,27,compile},{atom,27,forms}},[{call,27,{atom,27,forms},[{var,27,'Mod'},{var,27,'All'}]}]}]}]}]}]},{function,29,forms,2,[{clause,29,[{var,29,'Mod'},{var,29,'All'}],[],[{call,30,{remote,30,{atom,30,lists},{atom,30,append}},[{cons,30,{call,30,{atom,30,mk_attrs},[{var,30,'Mod'}]},{cons,30,{call,30,{atom,30,mk_all_0},[{var,30,'All'}]},{cons,30,{call,30,{atom,30,mk_get_1},[{var,30,'All'}]},{nil,30}}}},{call,30,{atom,30,mk_boilerplate},[]}]}]}]},{function,32,mk_attrs,1,[{clause,32,[{var,32,'Mod'}],[],[{cons,33,{tuple,33,[{atom,33,attribute},{integer,33,7},{atom,33,module},{var,33,'Mod'}]},{cons,34,{tuple,34,[{atom,34,attribute},{integer,34,9},{atom,34,export},{cons,34,{tuple,34,[{atom,34,all},{integer,34,0}]},{cons,34,{tuple,34,[{atom,34,get},{integer,34,1}]},{cons,34,{tuple,34,[{atom,34,set},{integer,34,2}]},{nil,34}}}}]},{nil,34}}}]}]},{function,36,mk_all_0,1,[{clause,36,[{var,36,'All'}],[],[{cons,37,{tuple,37,[{atom,37,function},{integer,37,11},{atom,37,all},{integer,37,0},{cons,38,{tuple,38,[{atom,38,clause},{integer,38,11},{nil,38},{nil,38},{cons,39,{call,39,{atom,39,mk_all_args},[{lc,39,{var,39,'K'},[{generate,39,{tuple,39,[{var,39,'K'},{var,39,'_'}]},{var,39,'All'}}]}]},{nil,39}}]},{nil,39}}]},{nil,39}}]}]},{function,41,mk_all_args,1,[{clause,41,[{cons,41,{var,41,'H'},{var,41,'T'}}],[],[{tuple,41,[{atom,41,cons},{integer,41,11},{tuple,41,[{atom,41,atom},{integer,41,11},{var,41,'H'}]},{call,41,{atom,41,mk_all_args},[{var,41,'T'}]}]}]},{clause,42,[{nil,42}],[],[{tuple,42,[{atom,42,nil},{integer,42,11}]}]}]},{function,44,mk_get_1,1,[{clause,44,[{var,44,'A'}],[],[{cons,45,{tuple,45,[{atom,45,function},{integer,45,13},{atom,45,get},{integer,45,1},{op,46,'++',{lc,46,{tuple,46,[{atom,46,clause},{integer,46,13},{cons,46,{tuple,46,[{atom,46,atom},{integer,46,13},{var,46,'K'}]},{nil,46}},{nil,46},{cons,46,{tuple,46,[{atom,46,string},{integer,46,13},{var,46,'V'}]},{nil,46}}]},[{generate,46,{tuple,46,[{var,46,'K'},{var,46,'V'}]},{var,46,'A'}}]},{cons,47,{tuple,47,[{atom,47,clause},{integer,47,14},{cons,47,{tuple,47,[{atom,47,var},{integer,47,14},{atom,47,'_'}]},{nil,47}},{nil,47},{cons,48,{tuple,48,[{atom,48,call},{integer,48,14},{tuple,48,[{atom,48,atom},{integer,48,14},{atom,48,exit}]},{cons,48,{tuple,48,[{atom,48,atom},{integer,48,14},{atom,48,not_found}]},{nil,48}}]},{nil,48}}]},{nil,48}}}]},{nil,48}}]}]}.

dbg() -> dbg(forms).
dbg(forms) -> forms(x,all());
dbg(abst) ->
  F = "/home/masse/git/cookbook/cb_getter_module",
  B = element(2,hd(element(2,element(2,beam_lib:chunks(F,["Abst"]))))),
  element(2,binary_to_term(B)).

%% [{attribute,1,file,{"./cb_getter_module.erl",1}},
%%  {attribute,7,module,cb_getter_module},
%%  {attribute,8,author,'Mats Cronqvist'},
%%  {attribute,9,export,[{all,0},{get,1},{set,2},{dbg,0}]},
%%  {function,11,all,0,
%%   [{clause,11,[],[],
%%     [{cons,11,
%%       {atom,11,a},
%%       {cons,11,{atom,11,b},{cons,11,{atom,11,c},{nil,11}}}}]}]},
%%  {function,13,get,1,
%%   [{clause,13,[{atom,13,a}],[],[{string,13,"A"}]},
%%    {clause,14,[{atom,14,b}],[],[{string,14,"B"}]},
%%    {clause,15,[{atom,15,c}],[],[{string,15,"C"}]}]},
%%  {function,17,set,2,
%%   [{clause,17,[{var,17,'K'},{var,17,'V'}],[],[{atom,17,ok}]}]},
%%  {function,19,gen,1,
%%   [{clause,19,
%%     [{var,19,'Mod'}],
%%     [],
%%     [{call,20,
%%       {remote,20,{atom,20,code},{atom,20,load_binary}},
%%       [{var,20,'Mod'},
%%        {string,21,[]},
%%        {call,22,
%%         {atom,22,element},
%%         [{integer,22,3},
%%          {call,22,{remote,22,{atom,22,compile},{atom,22,forms}},[]}]}]}]}]},
