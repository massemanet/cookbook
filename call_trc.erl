%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 19 Jul 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('blerg').
-author('mats cronqvist').
-export([go/0]).

go() ->
  [sleeper1()||_<-lists:seq(1,1000)].

sleeper1() ->
  sleep(10),
  sleeper2().

sleeper2() ->
  sleep(8).

sleep(N) ->  receive after N -> ok end.

%% FUNCS = fun(M) -> [{M,F,A}||{function,_,F,A,_}<-element(2,binary_to_term(element(2,hd(element(2,element(2,beam_lib:chunks(code:which(M),["Abst"])))))))]end.

%% f(SHOW),SHOW = fun(M,K)->[{M,F,A,R}||{M,F,A,{_,R}}<-[{M,F,A,erlang:trace_info({M,F,A},K)}||{M,F,A}<-FUNCS(M)],R=/=false,R=/=[],R=/=undefined]end.

%% f(START),START=fun(M,K)->erlang:trace(all,true,[call]),erlang:trace_pattern({M,'_','_'},true,[K])end.

%% f(STOP),STOP=fun(M,K)->erlang:trace(all,false,[call]),erlang:trace_pattern({M,'_','_'},false,[K])end.
