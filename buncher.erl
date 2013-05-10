%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 May 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('buncher').
-author('mats cronqvist').
-export([start/0,stop/0]).
-export([source/0]).

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
  [{bunch_time,1000}].

add_defaults(Opts) ->
  [{K,proplists:get_value(K,Opts,V)} || {K,V} <- default_opts()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% server
init() ->
  receive
    {go,[{bunch_time,BTime}]} ->
      set_time(BTime),
      mloop(BTime,acc_new())
  end.

mloop(BTime,Acc) ->
  receive
    {attach,Pid} -> mloop(BTime,acc_append(attaches,Pid,Acc));
    {item,Val}   -> mloop(BTime,acc_append(vals,Val,Acc));
    time         -> send(Acc),set_time(BTime),mloop(BTime,acc_new());
    quit         -> ok
  end.

send(Acc) ->
  case acc_get(vals,Acc) of
    [] -> ok;
    Vs -> lists:foreach(fun(P) -> P ! Vs end, acc_get(attaches,Acc))
  end.

set_time(BunchTime) ->
  erlang:send_after(BunchTime,self(),time).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% hide the implementation of the accumulator
acc_new() ->
  orddict:from_list([{attaches,[]},{vals,[]}]).

acc_get(K,A) ->
  orddict:fetch(K,A).

acc_append(K,V,A) ->
  orddict:append(K,V,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% test data source
source() ->
  spawn(fun loop/0).

loop() ->
  timer:sleep(300),
  buncher ! {item,element(3,now())},
  loop().
