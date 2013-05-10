%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 May 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('buncher').
-author('mats cronqvist').
-export([start/0,stop/0]).

stop() -> ?MODULE ! quit.

start() -> start([]).

start(Opts) ->
  case whereis(?MODULE) of
    undefined -> register(?MODULE,spawn(fun init/0));
    _         -> ok
  end,
  ?MODULE ! {go,add_defaults(Opts)},
  whereis(?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% options
default_opts() ->
  [].

add_defaults(Opts) ->
  [{K,proplists:get_value(K,Opts,V)} || {K,V} <- default_opts()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% server
init() ->
  receive
    {go,[]} ->
      set_time(),
      mloop(acc_new())
  end.

mloop(Acc) ->
  receive
    {attach,Pid} -> mloop(acc_append(attaches,Pid,Acc));
    {item,Val}   -> mloop(acc_append(vals,Val,Acc));
    time         -> send(Acc),set_time(),mloop(acc_new());
    quit         -> ok
  end.

send(Acc) ->
  case acc_get(vals,Acc) of
    [] -> ok;
    Vs -> lists:foreach(fun(P) -> P ! Vs end, acc_get(attaches,Acc))
  end.

set_time() ->
  erlang:send_after(1000,self(),time).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hide the implementation of the accumulator
acc_new() ->
  orddict:from_list([{attaches,[]},{vals,[]}]).

acc_get(K,A) ->
  orddict:fetch(K,A).

acc_append(K,V,A) ->
  orddict:append(K,V,A).
