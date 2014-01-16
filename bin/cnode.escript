#!/usr/bin/env escript
%%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%! -sname gnutte -setcookie secretcookie
%%% Created : 15 Jan 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

main(_) ->
  {ok,Hostname} = inet:gethostname(),
  Cnode = list_to_atom("c1@"++Hostname),
  Port = open_port({spawn,make_cmd()},[stderr_to_stdout,exit_status]),
  {y,Cnode} ! {x,self(),{foo,1}},
  io:fwrite("port - ~s",[receive {_Port,{data,Y}} -> Y after 1000 -> "to" end]),
  flush(),
  port_close(Port).

make_cmd() ->
  filename:dirname(escript:script_name()) ++ "/cnode 12345".

flush() ->
  io:fwrite("~p~n",[receive X -> X after 1000 -> "to" end]).
