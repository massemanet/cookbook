%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 27 Jun 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% a pretty much minimal example of how to usu mod_fun
%% @end

-module(mod_fun_demo).
-author('mats cronqvist').
-export([ start/0
         ,do/2]).

start() ->
  [inets:start() || not is_started(inets)],
  inets:start(httpd,conf()).

conf() ->
  Root = filename:join("/tmp",?MODULE),
  [{port, 8765},
   {server_name,atom_to_list(?MODULE)},
   {server_root,ensure(Root)},
   {document_root,ensure(Root)},
   {modules, [mod_alias,mod_fun,mod_get,mod_log]},
   {directory_index, ["index.html"]},
   {error_log,filename:join(ensure(Root),"errors.log")},
   {handler_function,{?MODULE,do}},
   {mime_types,[{"html","text/html"},
                {"css","text/css"},
                {"ico","image/x-icon"},
                {"js","application/javascript"}]}].

is_started(A) -> lists:member(A,[X||{X,_,_}<-application:which_applications()]).

ensure(X) ->
  filelib:ensure_dir(X++"/"),
  X.

%% called from mod_fun. runs in a fresh process.
%% Req is a dict with the request data from inets. It is implemented
%% as a fun/1, with the arg being the key in the dict.
%% we can deliver the content in chunks by calling Act(Chunk).
%% the first chunk can be headers; [{Key,Val}]
%% if we don't want to handle the request, we do Act(defer)
%% if we crash, there will be a 404.
do(Act,Req) ->
  case is_tick(Req) of
    false -> Act("Zzzz...");
    true  -> Act(ticker())
  end.

is_tick(Req) ->
  Req(request_uri) =:= "/tick" andalso Req(method) =:= "GET".

ticker() ->
  T = round(1000-element(3,now())/1000),
  receive
  after T ->
      {{Y,Mo,D},{H,Mi,S}} = calendar:now_to_local_time(now()),
      io_lib:fwrite("~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w",[Y,Mo,D,H,Mi,S])
  end.
