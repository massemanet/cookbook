%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  7 Mar 2015 by  <masse@klarna.com>

%% @doc
%% @end

-module('stream').
-export([new/1,new/2,new/3,next/1,fold/3]).

new(Next)           -> new(Next,nil).
new(Next,Init)      -> new(Next,Init,nil).
new(Next,Init,Exit) -> {stream,maybe_init(Init),Next,Exit}.

next({stream,State,Next,Exit}) ->
  try
    {Element,Nstate} = Next(State),
    {Element,{stream,Nstate,Next,Exit}}
  catch
    throw:finished -> maybe_exit(Exit)
  end.

fold(Fun,Acc,{stream,State,Next,Exit}) ->
  case next({stream,State,Next,Exit}) of
    {El,Nstream} ->
      case nacc(El,Fun,Acc,Exit) of
        {ok,Nacc} -> fold(Fun,Nacc,Nstream);
        finished -> Acc
      end;
    finished -> Acc
  end.

nacc(El,Fun,Acc,Exit) ->
  try {ok,Fun(El,Acc)}
  catch
    C:R ->
      maybe_exit(Exit),
      case {C,R} of
        {throw,finished} -> finished;
        {throw,_}        -> throw(R);
        {_,_}            -> exit({R,erlang:get_stacktrace()})
      end
  end.

maybe_init(Init) when is_function(Init) -> Init();
maybe_init(_) -> nil.

maybe_exit(Exit) when is_function(Exit) -> Exit(),finished;
maybe_exit(_) -> finished.
