%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 26 Jun 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% encode positive integers in 2 =< base =< 62
%% @end

-module('radix').
-author('mats cronqvist').
-export([enc/2]).

enc(0,_) -> "0";
enc(Int,Base) ->
  enc(Int,Base,trunc(math:log(Int+0.1)/math:log(Base)),[]).

enc(0,_,N,R) when N < 0 ->
  [anum(E) || E <- lists:reverse(R)];
enc(0,B,N,R) ->
  enc(0,B,N-1,[0|R]);
enc(I,B,N,R) ->
  P = round(math:pow(B,N)),
  M = I div P,
  enc(I-M*P,B,N-1,[M|R]).

anum(E) when E =< 9 -> E+$0;
anum(E) when E =< 36 -> E+$A-10;
anum(E) when E =< 62 -> E+$a-37.
