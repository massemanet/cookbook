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
%% There's only one interesting function; yawn:start/1
%% The argument is a proplist. The available tags are;
%%   port - the port number [6666]
%%   packeting - as per erlang:decode_packet/3 [http_bin]
%%   handler - a fun/3 [yawn:handler/3]
%%
%% The handler fun is called when there is a data on the socket. The args are;
%%   Packeting - tcp | http
%%   Data - term(); whatever erlang:decode_packet/3 returned.
%%   State - term(); the previous handler state. Initialized to [].
%% The handler should return one of these;
%%   close - no reply, close the socket.
%%   {close,Reply} - send Reply, then close.
%%   {keep,State} - send no reply, don't close the socket.
%%   {keep,Reply,State} - send Reply, don't close the socket.
%%
%% Examples;
%%    yawn:start().
%%  will start a server listening to port 6666, expecting http data, and
%%  replying with a representation of the http request.
%%
%%    yawn:start([{packeting,raw}]).
%%  will start an echo server on port 6666.
%%  If you connect with telnet ("telnet localhost 6666"), it will echo
%%  whatever you type in until you type "close" (then it will close the socket).
%%
%%    yawn:start([{packeting,raw},{handler,H}]).
%%  where
%%   H=fun(_,<<"q\r\n">>,_)->close;(_,X,[])->{keep,"",X};(_,X,S)->{keep,S,X}end.
%%  will start a server that echoes its previous input, and stops when its
%%  input is "q".
%% @end

-module('yawn').
-author('mats cronqvist').

-export([start/0,start/1,stop/0]). % API

-export([listen_loop/1,worker_loop/3]). % internal exports

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the API
stop() ->
  case whereis(yawn) of
    undefined -> ok;
    Pid       -> exit(Pid,kill)
  end.

start() ->
  start([]).

start(Opts) ->
  case whereis(yawn) of
    undefined -> register(yawn,spawn(fun init/0));
    _         -> ok
  end,
  yawn ! {go,add_defaults(Opts)},
  whereis(yawn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% options
default_opts() ->
  [{port,6666},
   {packeting,http_bin},
   {handler,fun handler/3}].

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
init() ->
  receive
    {go,Opts} ->
      try init(Opts)
      catch error:R -> log({error,R})
      end
  end.

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
      Worker ! {Socket,get_opt(handler,Opts)},
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
    {Socket,Handler} -> worker_loop(Socket,Handler,[])
  end.

worker_loop(Socket,H,HState) ->
  inet:setopts(Socket,[{active,once}]),
  receive
    {tcp_closed,Socket} -> ok;
    {tcp_error, Socket, R} -> log({socket_closed,Socket,R});
    {Packeting,Socket,Data} ->
      case H(Packeting,Data,HState) of
        close           -> ok;
        {keep,HS}       -> ?MODULE:worker_loop(Socket,H,HS);
        {close,Reply}   -> send(Socket,Reply);
        {keep,Reply,HS} -> send(Socket,Reply),?MODULE:worker_loop(Socket,H,HS)
      end
  end.

send(Socket,Reply) ->
  case gen_tcp:send(Socket,Reply) of
    {error,R} -> log({error_sending,R,Reply});
    ok -> ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% the default handler
-define(HttpEoh(),http_eoh).
-define(HttpRequest(Method,Uri,Vsn),{http_request,Method,Uri,Vsn}).
-define(HttpHeader(Field,Value),{http_header,_,Field,_,Value}).
-define(HttpError(Value),{http_error,Value}).

handler(tcp,<<"close\r",_/binary>>,_)    -> close;
handler(tcp,Data,S)                      -> {keep,Data,S};
handler(http,?HttpEoh(),S)               -> {keep,S};
handler(http,?HttpError(Val),_)          -> {close,flat(Val)};
handler(http,?HttpHeader(_,_),S)         -> {keep,S};
handler(http,?HttpRequest(Meth,Uri,_),_) -> {close,flat({Meth,Uri})}.

flat(Term) ->
  lists:flatten(io_lib:fwrite("~p",[Term])).
