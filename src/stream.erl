%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  7 Mar 2015 by  <masse@klarna.com>

%% @doc
%% @end

-module('stream').
-export([new/2,next/1,fold/3]).

new(Init,Next) -> {stream,Init(),Next}.

next({stream,State,Next}) ->
  try
    {Element,Nstate} = Next(State),
    {Element,{stream,Nstate,Next}}
  catch
    throw:finished -> finished
  end.

fold(Fun,Acc,Stream) ->
  case Stream:next() of
    {El,Nstream} -> fold(Fun,Fun(El,Acc),Nstream);
    finished -> Acc
  end.
