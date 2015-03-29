%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  4 Sep 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% Yet Another Webserver, Not.
%%
%% There are already hundreds of web servers written in Erlang.
%% yawn is a demo of an Erlang TCP socket server, that happens to include an
%% embryo of a web server. It is in no way intended for production.
%%
%% Design-wise, there is one process, with the registered name 'yawn', that
%% listens. When a client connects, we spawn_link a worker and transfer the
%% socket to it. The socket is never closed; instead the worker just exits.
%% This is optimized for relatively long-lived workers. But simple.
%%
%% There's only one interesting function; yawn:start/2
%% The first argument is an atom() - an identifier of this instance of yawn.
%% The second argument is a proplist. The available tags are;
%%   port - the port number [9012]
%%   packeting - as per erlang:decode_packet/3 [http_bin]
%%   handler - a fun/4 [yawn:handler/4]
%%
%% The handler fun is called when there is a data on the socket. The args are;
%%   Packeting - 'tcp' | 'http_bin'
%%   Type - the initial packeting type.
%%   Data - term(); whatever erlang:decode_packet/3 returned.
%%   State - term(); the previous handler state. Initialized to [].
%% The handler is also called if the worker process receives data that does
%% not come from the socket. In that case the args are;
%%   Packeting - 'msg'
%%   Data - term();
%%   State -term(); the handler state
%% The handler should return one of these;
%%   close - no reply, close the socket.
%%   {close,Reply} - send Reply, then close.
%%   {keep,State} - send no reply, don't close the socket.
%%   {keep,Reply,State} - send Reply, don't close the socket.
%%
%% Examples;
%%    yawn:start(x).
%%  will start a server listening to port 9012, expecting http data, and
%%  replying with a representation of the http request.
%%
%%    yawn:start(x,[{packeting,raw}]).
%%  will start an echo server on port 9012.
%%  If you connect with telnet ("telnet localhost 9012"), it will echo
%%  whatever you type in until you type "close" (then it will close the socket).
%%
%%    yawn:start(y,[{packeting,raw},{handler,H}]).
%%  where
%%   H=fun(_,<<"q\r\n">>,_)->close;(_,X,[])->{keep,"",X};(_,X,S)->{keep,S,X}end.
%%  will start a server that echoes its previous input, and stops when its
%%  input is "q".
%% @end

-module('yawn').
-author('mats cronqvist').

-export([start/1,start/2,start_link/2,stop/1]). % API

-export([listen_loop/1,worker_loop/5]). % internal exports

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the API
stop(Name) ->
  case whereis(Name) of
    undefined -> ok;
    Pid       -> exit(Pid,kill)
  end.

start_link(Name,Opts) ->
  Pid = start(Name,Opts),
  true = link(Pid),
  {ok,Pid}.

start(Name) ->
  start(Name,[]).

start(Name,Opts) ->
  Os = add_defaults(Opts),
  Self = self(),
  {Pid,Ref} = spawn_monitor(fun() -> init(Name,Os,Self) end),
  receive
    {'DOWN',Ref,_,Pid,badarg} -> {yawn,already_started};
    {'DOWN',Ref,_,Pid,R}      -> {yawn,R};
    {ok,Pid}                  -> demonitor(Ref,[flush,info])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% options
default_opts() ->
  [{port,9012},
   {packeting,http_bin},
   {handler,fun handler/4}].

add_defaults(Opts) ->
  [{K,proplists:get_value(K,Opts,V)} || {K,V} <- default_opts()].

get_opt(Opt,Opts) ->
  proplists:get_value(Opt,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the logger
log(X) ->
  erlang:display(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the main process
init(Name,Opts,Daddy) ->
  try
    register(Name,self())
  catch
    _:_ -> exit(already_started)
  end,
  Daddy ! {ok,self()},
  init(Opts).

init(Opts) ->
  process_flag(trap_exit,true),
  case gen_tcp:listen(get_opt(port,Opts),socket_opts(Opts)) of
    {error,R} -> log({error_listening,R,Opts});
    {ok,LS} -> listen_loop([{listen_socket,LS}|Opts])
  end.

listen_loop(Opts) ->
  case gen_tcp:accept(get_opt(listen_socket,Opts)) of
    {error,R}   ->
      log({reopening_listener,R}),
      init(Opts);
    {ok,Socket} ->
      Worker = spawn_link(fun worker/0),
      Worker ! {init,Socket,get_opt(handler,Opts),get_opt(packeting,Opts)},
      socket_handover(Socket,Worker),
      flush_exits(),
      ?MODULE:listen_loop(Opts)
  end.

socket_handover(Socket,Worker) ->
  case gen_tcp:controlling_process(Socket,Worker) of
    {error,R} -> log({error_handover,R});
    ok -> ok
  end.

flush_exits() ->
  receive
    {'EXIT',_,normal} -> ok;
    {'EXIT',_,E} -> log({child_died,E})
  after
    0 -> ok
  end.

socket_opts(Opts) ->
  [{active,false},
   {reuseaddr,true},
   binary,
   {packet,get_opt(packeting,Opts)}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the worker
worker() ->
  receive
    {init,Socket,Handler,Pack} -> worker_loop(Socket,Pack,Handler,[],Pack)
  end.

worker_loop(Socket,Type,H,HState,Pack) ->
  inet:setopts(Socket,[{active,once},{packet,Pack}]),
  case recv(Socket,Type,H,HState) of
    {loop,HS,P} -> ?MODULE:worker_loop(Socket,Type,H,HS,P);
    close       -> ok
  end.

recv(Socket,Type,H,HState) ->
  receive
    {tcp_closed,Socket}  -> ok;
    {tcp_error,Socket,R} -> log({socket_closed,Socket,R});
    {Pack,Socket,Data}   -> handle(Socket,Type,Pack,Data,H,HState);
    Data                 -> handle(Socket,Type,msg,Data,H,HState)
  end.

handle(Socket,Type,Pack,Data,H,HState) ->
  case H(Pack,Type,Data,HState) of
    close           -> close;
    {keep,HS}       -> {loop,HS,Type};
    {keep_p,HS,P}   -> {loop,HS,P};
    {close,Reply}   -> send(Socket,Reply),close;
    {keep,Reply,HS} -> send(Socket,Reply),{loop,HS,Type}
  end.

send(Socket,Reply) ->
  case gen_tcp:send(Socket,Reply) of
    {error,R} -> log({error_sending,R,Reply});
    ok -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the default handler
-define(HttpEoh(),              http_eoh).
-define(HttpReq(Method,Uri,Vsn),{http_request,Method,Uri,Vsn}).
-define(HttpHdr(Field,Value),   {http_header,_,Field,_,Value}).
-define(HttpErr(Value),         {http_error,Value}).

handler(tcp,raw,<<"close\r",_/binary>>,_)     -> close;
handler(tcp,raw,Data,S)                       -> {keep,Data,S};
handler(tcp,http_bin,Data,{Rq,Hs})            -> {keep_p,{Rq,Hs,Data},http_bin};
handler(http,http_bin,?HttpReq(Meth,Uri,_),_) -> {keep,{{Meth,Uri},[]}};
handler(http,http_bin,?HttpHdr(K,V),{Rq,Hs})  -> {keep,{Rq,[{K,V}|Hs]}};
handler(http,http_bin,?HttpErr(Val),_)        -> {close,flat(Val)};
handler(http,http_bin,?HttpEoh(),S)           ->
  case has_body(S) of
    false-> {close,flat(S)};
    true -> {keep_p,S,raw}
  end.

%% need to check the headers (in S) to see if there's a body
has_body(_) ->
  false.

flat(Term) ->
  lists:flatten(io_lib:fwrite("~p",[Term])).
