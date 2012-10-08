%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Jun 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% a pretty much minimal example of how to start an inets http server
%% @end

-module('cb_inets').
-author('mats cronqvist').
-export([ start/0
         ,do/3]).

%%   Starts a server that serves static files from <server_root>
%%   where <server_root> is /tmp/?MODULE
%%   It logs errors to <server_root>/errors.log
%%   When the url is localhost:8989 it will serve <server_root>/index.html
%%   Try e.g. putting this; "<b>bold</b><p><a href='erl/cb_inets/do'>link</a>"
%% in <server_root>/index.html
%%   When the url is localhost:8989/erl/<?MODULE>/do[?/]*
%% it will serve whatever ?MODULE:do/3 returns.
start() ->
  inets:stop(),
  inets:start(),
  inets:start(httpd, conf()).

conf() ->
  Root = filename:join("/tmp",?MODULE),
  [{port, 8989},
   {server_name,flat(?MODULE)},
   {server_root,ensure(Root)},
   {document_root,ensure(Root)},
   {modules, [mod_alias,mod_esi,mod_get,mod_log]},
   {error_log,ensure(filename:join(Root,"errors.log"))},
   {directory_index, ["index.html"]},
   {erl_script_alias, {"/erl", [?MODULE]}},
   {erl_script_nocache,true}].

ensure(X) ->
  filelib:ensure_dir(X++"/"),
  X.

%% called when the server sees /erl/<?MODULE>/do[/?]*
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
