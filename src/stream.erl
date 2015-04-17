%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  7 Mar 2015 by  <masse@klarna.com>

%% @doc
%% @end

-module('stream').
-export([new/1,new/2,new/3,next/1,fold/3]).

-record(stream,{state=nil,next,exit=nil}).

new(Next)           -> new(Next,nil).
new(Next,Init)      -> new(Next,Init,nil).
new(Next,Init,Exit) -> #stream{state=maybe_init(Init),next=Next,exit=Exit}.

next(S) ->
  try
    {Element,Nstate} = (S#stream.next)(S#stream.state),
    {Element,S#stream{state=Nstate}}
  catch
    throw:finished -> maybe_exit(S)
  end.

fold(Fun,Acc,S) ->
  case next(S) of
    {El,NS} ->
      case nacc(El,Fun,Acc,NS) of
        {ok,Nacc} -> fold(Fun,Nacc,NS);
        finished -> Acc
      end;
    finished -> Acc
  end.

nacc(El,Fun,Acc,S) ->
  try {ok,Fun(El,Acc)}
  catch
    C:R ->
      maybe_exit(S),
      case {C,R} of
        {throw,finished} -> finished;
        {throw,_}        -> throw(R);
        {_,_}            -> exit({R,erlang:get_stacktrace()})
      end
  end.

maybe_init(Init) when is_function(Init) -> Init();
maybe_init(_) -> nil.

maybe_exit(S) when is_function(S#stream.exit) -> stream_exit(S),finished;
maybe_exit(_) -> finished.

stream_exit(S) -> (S#stream.exit)(S#stream.state).
