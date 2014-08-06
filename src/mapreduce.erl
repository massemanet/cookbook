%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  6 Aug 2014 by  <masse@klarna.com>

%% @doc
%% @end

-module('mapreduce').
-author('').
-export([parallel/2]).

% run N parallel instances of F/0
parallel(F,N) ->
  reduce([spawn_monitor(fun() -> map(F) end) || _ <- lists:seq(1,N)]).

map(F) ->
  exit(F()).

reduce([]) -> [];
reduce(PidRefs) ->
  receive
    {'DOWN',Ref,_,Pid,Reason} -> [Reason|reduce(PidRefs--[{Pid,Ref}])]
  end.
