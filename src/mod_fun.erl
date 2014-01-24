%% A plugin to enable a somewhat sane web server programming
%% experience on top of inets. It is still much worse than
%% webmachine/mochiweb. Use that if you can! This code could only
%% concievably be useful for people who, for some fiendish reason, is
%% stuck with inets. Pity those fools.
%%
-module(mod_fun).
%% the rather grandiosely named "ERLANG WEB SERVER CALLBACK API"
-export([do/1, load/2, store/2]).

-include_lib("inets/include/httpd.hrl").
-include_lib("inets/src/http_server/httpd_internal.hrl").
-include_lib("inets/src/inets_app/inets_internal.hrl").

-define(VMODULE,"FUN").    % what the hell does this do? snmp??

%% the configuration parameters
default(handler_timeout) -> 5000;
default(handler_function) -> {"",""}.

%% "ERLANG WEB SERVER API CALLBACK FUNCTIONS"
load("HandlerFunction " ++ HandlerFunction, []) ->
  try
    [Mod,Fun] = string:tokens(HandlerFunction," :"),
    {ok, [], {handler_function, {list_to_atom(Mod),list_to_atom(Fun)}}}
  catch _:_ ->
      {error, ?NICE(HandlerFunction ++ " is an invalid HandlerFunction")}
  end;
load("HandlerTimeout " ++ HandlerTimeout, []) ->
  try
    [TO] = string:tokens(HandlerTimeout," "),
    {ok, [], {handler_timeout, {list_to_integer(TO)}}}
  catch _:_ ->
      {error, ?NICE(HandlerTimeout ++ " is an invalid HandlerTimeout")}
  end.

store({handler_function, {M,F}} = Conf, _) when is_atom(M),is_atom(F)->
  {ok, Conf};
store({handler_integer, TO} = Conf, _) when is_integer(TO)->
  {ok, Conf}.

do(ModRec) ->
  case defer_response(ModRec) of
    false-> {proceed, safe_handle(ModRec)};
    true -> {proceed, ModRec#mod.data}
  end.

%% we guarantee that handle/1 succeeds (in which case we send a 200) or we
%% generate a 404.
safe_handle(ModRec) ->
  try handle(ModRec)
  catch _:_ -> fourofour(ModRec)
  end.

%% true if some other mod_* has already handled the request
defer_response(#mod{data=Data}) ->
  (proplists:get_value(response,Data) =/= undefined) orelse
    (proplists:get_value(status,Data) =/= undefined).

%%%========================================================================
%% since the handler fun can send many chunks, or not used chunked
%%  encoding at all,  we keep state while waiting.
%% there are three states;
%% init - no data received yet
%% has_headers - we have received the headers, but not sent them
%% sent_headers - we have received and sent headers.
-record(s,{state=init,
           chunks=[],
           headers=[],
           path="",
           length=0,
           chunked_send_p,
           timeout}).

%% should we do chunked sending
chunked_send_p(#mod{config_db=Db,http_version=HTTPV}) ->
  (HTTPV =/= "HTTP/1.1") orelse httpd_response:is_disable_chunked_send(Db).

%% we spawn into the handler fun, monitors it, and waits for data chunks.
handle(ModRec) ->
  Self = self(),
  {M,F} = mod_get(ModRec,handler_function),
  S = #s{chunked_send_p=chunked_send_p(ModRec),
         timeout=mod_get(ModRec,handler_timeout)},
  Act = fun(defer) -> exit(defer);(L) -> Self ! {self(),L} end,
  Mod = lists:zip(record_info(fields,mod),tl(tuple_to_list(ModRec))),
  Req = fun(all) -> proplists:unfold(Mod);
           (Key) -> proplists:get_value(Key,Mod) end,
  loop(spawn_monitor(fun() -> M:F(Act,Req) end),S,ModRec).

mod_get(ModRec,Key) ->
  httpd_util:lookup(ModRec#mod.config_db,Key,default(Key)).

loop({Pid,Ref},S,ModRec) ->
  Timeout = S#s.timeout,
  receive
    {Pid,Chunk} -> loop({Pid,Ref},chunk(Chunk,S,ModRec),ModRec);
    {'DOWN',Ref,_,Pid,Reason} ->
      case {S#s.state,Reason} of
        {sent_headers,defer} -> twohundred(ModRec,S);
        {_,defer}            -> ModRec#mod.data;
        {init,normal}        -> fourofour(ModRec);
        {_,normal}           -> twohundred(ModRec,S);
        {sent_headers,_}     -> twohundred(ModRec,S);
        {_,_}                -> fourofour(ModRec)
      end
  after
    Timeout ->
      exit(Pid,kill),
      loop({Pid,Ref},S,ModRec)
  end.

%% all went well. response is 200.
twohundred(ModRec,S) ->
  case S#s.state of
    has_headers -> send_unchunked(200,ModRec,S#s.headers,S#s.chunks);
    sent_headers-> send_final_chunk(ModRec)
  end,
  [{response, {already_sent, 200, S#s.length}} | ModRec#mod.data].

fourofour(ModRec) ->
  Len = send_unchunked(404,ModRec,[{"connection","close"}],"nothing here."),
  [{response,{already_sent,404,Len}} | ModRec#mod.data].

%% got a chunk. it's either headers or a body part.
%% if we don't get headers first time, use default headers.
%% if we're not usung chunked encoding, stash everything.
%% if we are using chunked encoding, send every chunk we get.
chunk(Chunk,S,ModRec) ->
  case S#s.state of
    init ->
      {Headers,Body} = check_headers(Chunk),
      case S#s.chunked_send_p of
        true ->
          send_headers(true,ModRec,Headers),
          send_chunk(ModRec,Body),
          Len = S#s.length + length(Body),
          S#s{state=sent_headers,chunks=[],length=Len};
        false->
          S#s{state=has_headers,headers=Headers,chunks=Body}
      end;
    sent_headers ->
      Len = S#s.length + length(Chunk),
      send_chunk(ModRec,Chunk),
      S#s{length=Len};
    has_headers ->
      S#s{chunks=S#s.chunks++Chunk};
    _ ->
      S
  end.

check_headers(Chunk) ->
  case is_headers(Chunk) of
    true -> {Chunk,""};
    false-> {[{"content-type","text/html"}],Chunk}
  end.

is_headers([{_,_}|L]) -> is_headers(L);
is_headers([]) -> true;
is_headers(_) -> false.

%%%% send stuff
%% not chunking
send_headers(false,ModRec,Headers) ->
  send_headers(ModRec,[{"connection","close"} | Headers]);
%% chunking
send_headers(true,ModRec,Headers) ->
  send_headers(ModRec,[{"transfer-encoding","chunked"} | Headers]).

%% wrapper around httpd_response
send_headers(ModRec,HTTPHeaders) ->
  ExtraHeaders = read_header_cache(ModRec),
  httpd_response:send_header(ModRec,200,ExtraHeaders++HTTPHeaders).

send_chunk(ModRec,Chunk) ->
  httpd_response:send_chunk(ModRec,Chunk,false).

send_final_chunk(ModRec) ->
  httpd_response:send_final_chunk(ModRec,false).

send_unchunked(Status,ModRec,Headers,Body) ->
  Len = integer_to_list(lists:flatlength(Body)),
  send_headers(false,ModRec,[{"content-length",Len} | Headers]),
  httpd_response:send_body(ModRec,Status,Body),
  Len.

read_header_cache(ModRec) ->
  try httpd_response:cache_headers(ModRec)                % -R15
  catch
    _:undef ->
      httpd_response:cache_headers(ModRec,script_nocache) % R16-
  end.
