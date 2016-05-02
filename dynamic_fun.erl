%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  2 May 2016 by masse <mats.cronqvist@gmail.com>

%% @doc
%% make a dynamic fun from a string.
%% <arglist> -> <expression>[,<expression>]
%% E.g. Y,X->2*X*Y
%% @end

-module('dynamic_fun').
-author('masse').
-export([make/1]).

make(Str) ->
  {ok,Toks,_} = erl_scan:string(Str),
  FunToks = decorate(Toks),
  {ok,[Form]} = erl_parse:parse_exprs(FunToks),
  erl_eval:expr(Form, [], none, none, value).

decorate(Toks) -> decorate(Toks,[{'(',1},{'fun',1}]).

decorate([{'->',L}|Toks],Acc) -> decorate(Toks,[{'->',L},{')',L}|Acc]);
decorate([{T,L}],Acc) -> lists:reverse([{dot,L},{T,L}|Acc]);
decorate([{T,L,D}],Acc) -> lists:reverse([{dot,L},{'end',L},{T,L,D}|Acc]);
decorate([H|T],Acc) -> decorate(T,[H|Acc]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

t0_test() ->
  Fun = make("Y,X->2*X*Y"),
  '-expr/5-fun-5-' = proplists:get_value(name,erlang:fun_info(Fun)),
  24 = Fun(3,4).

-endif.
