#!/usr/bin/env escript
%%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%! -sname gnutte
%%% Created : 15 Jan 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

main(_) ->
  {ok,Hostname} = inet:gethostname(),
  Cnode = list_to-atom("c1@"++Hostname),
  open_port({spawn,make_cmd()},[stderr_to_stdout,exit_status]),
  {y,Cnode} ! {x,self(),{foo,1}}.
  io:fwrite("~s~n",[receive {_Port,{data,X}} -> X after 1000 -> "to" end]).
make_cmd() ->
  "../bin/cnode 12345".
