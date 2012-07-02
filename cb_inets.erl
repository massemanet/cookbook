%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Jun 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% a pretty much minimal example of how to start an inets http server
%% @end

-module('cb_inets').
-author('mats cronqvist').
-export([ start/0
         ,do/3]).

%% starts a server that serves localhost:8989/<module>/do[?/]*
%% logs errors to /tmp/<module>/errors.log
start() ->
  inets:stop(),
  inets:start(),
  Root = filename:join("/tmp",?MODULE),
  inets:start(httpd, [{port, 8989},
                      {server_name,flat(?MODULE)},
                      {server_root,ensure(Root++"/")},
                      {document_root,ensure(Root++"/")},
                      {modules, [mod_esi,mod_log]},
                      {error_log,ensure(filename:join(Root,"errors.log"))},
                      {erl_script_alias, {"", [?MODULE]}},
                      {erl_script_nocache,true}]).

%% called when the server sees /<module>/do[/?]*
%% we can deliver the content in chunks, as long as do/3 does not return
do(SessionID,Env,Input) ->
  mod_esi:deliver(SessionID,
                  ["Content-Type: text/html\r\n\r\n", 
                   "<html><title>I am ",
                   flat(node()),
                   "</title><body><h2>",
                   flat(?MODULE),
                   "</h2>Input:<tt>",
                   flat(Input),
                   "</tt>"]),
  mod_esi:deliver(SessionID,
                  ["<br>Env:",
                   flat(Env),
                   "</body></html>"]).

flat(X) ->
  lists:flatten(io_lib:fwrite("~p",[X])).

ensure(X) ->
  filelib:ensure_dir(X),
  X.
