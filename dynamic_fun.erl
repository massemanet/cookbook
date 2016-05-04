%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  2 May 2016 by masse <mats.cronqvist@gmail.com>

%% @doc
%% make a dynamic fun from a string.
%% (<arglist>) -> <expression>[,<expression>]
%% E.g. (Y,X)->2*X*Y
%% @end

-module('dynamic_fun').
-author('masse').
-export([make/1]).

make(Str) ->
  {ok,Toks,L} = erl_scan:string(Str),
  FunToks = decorate(Toks,L),
  {ok,[Form]} = erl_parse:parse_exprs(FunToks),
  erl_eval:expr(Form, [], none, none, value).

decorate(Toks,L) ->
  lists:reverse(decorat(Toks,[{'fun',L}])).

decorat([{T,L}],Acc)         -> [{dot,L},{'end',L},{T,L}|Acc];
decorat([{T,L,D}],Acc)       -> [{dot,L},{'end',L},{T,L,D}|Acc];
decorat([H|T],Acc)           -> decorat(T,[H|Acc]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

t0_test() ->
  Fun = make("(Y,X)->2*X*Y"),
  '-expr/5-fun-5-' = proplists:get_value(name,erlang:fun_info(Fun)),
  24 = Fun(3,4).

t1_test() ->
  Fun = make("(Y,0)->2*Y*Y; (Y,X)->3*X*Y"),
  18 = Fun(3,0),
  36 = Fun(3,4).

t2_test() ->
  Fun = make("(Y,X)->ok"),
  ok = Fun(3,4).

-endif.
