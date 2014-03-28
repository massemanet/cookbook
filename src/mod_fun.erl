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

-ignore_xref([{httpd_response,cache_headers,1},
              {httpd_response,cache_headers,2}]).

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

do(M) ->
  case defer_response(M) of
    false-> {proceed, safe_handle(M)};
    true -> {proceed, M#mod.data}
  end.

%% we guarantee that handle/1 succeeds (in which case we send a 200) or we
%% generate a 404.
safe_handle(M) ->
  try handle(M)
  catch _:_ -> fourohfour(M,[])
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
           mime_type,
           timeout}).

%% we spawn into the handler fun, monitors it, and waits for data chunks.
handle(M) ->
  Self = self(),
  {Mod,Fun} = mod_get(M,handler_function),
  Act = fun(defer)          -> exit(defer);
           ({redirect,URL}) -> exit({redirect,URL});
           (L)              -> Self ! {self(),L}
        end,
  Mpl = lists:zip(record_info(fields,mod),tl(tuple_to_list(M))),
  Req = fun(all) -> proplists:unfold(Mpl);
           (Key) -> proplists:get_value(Key,Mpl) end,
  S = #s{chunked_send_p=chunked_send_p(M),
         timeout=mod_get(M,handler_timeout),
         mime_type=mime_type(Req(request_uri),M)},
  loop(spawn_monitor(fun() -> Mod:Fun(Act,Req) end),S,M).

mime_type(URI,M) ->
  httpd_util:lookup_mime_default(M#mod.config_db,suffix(URI),"text/html").

suffix(URI) ->
  try tl(filename:extension(URI))
  catch _:_ -> []
  end.

mod_get(M,Key) ->
  httpd_util:lookup(M#mod.config_db,Key,default(Key)).

loop({Pid,Ref},S,M) ->
  Timeout = S#s.timeout,
  receive
    {Pid,Chunk} ->
      loop({Pid,Ref},chunk(Chunk,S,M),M);
    {'DOWN',Ref,_,Pid,Reason} ->
      case {S#s.state,Reason} of
        {has_headers,normal} -> twohundred(M,S);
        {sent_headers,_}     -> twohundred(M,S);
        {_,defer}            -> M#mod.data;
        {_,{redirect,URL}}   -> threeohone(M,S,URL);
        {_,_}                -> fourohfour(M,S)
      end
  after
    Timeout ->
      exit(Pid,kill),
      loop({Pid,Ref},S,M)
  end.

%% all went well. response is 200.
twohundred(M,S) ->
  case S#s.state of
    has_headers -> send_unchunked(200,M,S#s.headers,S#s.chunks);
    sent_headers-> send_final_chunk(M)
  end,
  [{response, {already_sent, 200, S#s.length}} | M#mod.data].

fourohfour(M,S) ->
  send_status(M,S,404,"This is a 404",[]).

threeohone(M,S,URL) ->
  send_status(M,S,301,"Redirect to "++URL,[{"Location",URL}]).

send_status(M,S,Status,Response,Headers) ->
  case is_record(S,s) andalso S#s.state == sent_headers of
    false-> send_unchunked(Status,M,Headers,Response);
    true -> send_final_chunk(M)
  end,
  [{response, {already_sent, Status, length(Response)}} | M#mod.data].

%% got a chunk. it's either headers or a body part.
%% if we don't get headers first time, use default headers.
%% if we're not usung chunked encoding, stash everything.
%% if we are using chunked encoding, send every chunk we get.
chunk(Chnk,S,M) ->
  Chunk = to_list(Chnk),
  case S#s.state of
    init ->
      {Headers,Body} = check_headers(Chunk,S),
      case S#s.chunked_send_p of
        true ->
          send_header(M,200,[{"transfer-encoding","chunked"}|Headers]),
          send_chunk(M,Body),
          Len = S#s.length + length(Body),
          S#s{state=sent_headers,chunks=[],length=Len};
        false->
          S#s{state=has_headers,headers=Headers,chunks=Body}
      end;
    sent_headers ->
      Len = S#s.length + length(Chunk),
      send_chunk(M,Chunk),
      S#s{length=Len};
    has_headers ->
      S#s{chunks=S#s.chunks++Chunk};
    _ ->
      S
  end.

check_headers(Chunk,S) ->
  case is_headers(Chunk) of
    true -> {Chunk,""};
    false-> {[{"content-type",S#s.mime_type}],Chunk}
  end.

is_headers([{_,_}|L]) -> is_headers(L);
is_headers([]) -> true;
is_headers(_) -> false.

to_list(L) when is_list(L) -> L;
to_list(B) when is_binary(B) -> binary_to_list(B).

%% wrapper around httpd_response

%% can we do chunked sending?
chunked_send_p(#mod{config_db=Db,http_version=HTTPV}) ->
  chunked_send_p(HTTPV,httpd_response:is_disable_chunked_send(Db)).
chunked_send_p(HTTPV,Disabled) ->
  (HTTPV =/= "HTTP/0.9") andalso (HTTPV =/= "HTTP/1.0") andalso (not Disabled).

send_chunk(M,Chunk) ->
  httpd_response:send_chunk(M,Chunk,false).

send_final_chunk(M) ->
  httpd_response:send_final_chunk(M,false).

send_unchunked(Status,M,Headers,Body) ->
  L = integer_to_list(lists:flatlength(Body)),
  send_header(M,Status,[{"content-length",L},{"connection","close"}|Headers]),
  httpd_response:send_body(M,Status,Body),
  L.

send_header(M,Status,HTTPHeaders) ->
  ExtraHeaders = read_header_cache(M),
  httpd_response:send_header(M,Status,ExtraHeaders++HTTPHeaders).

read_header_cache(M) ->
  try httpd_response:cache_headers(M,script_nocache)   % R16-
  catch _:undef -> httpd_response:cache_headers(M)     % -R15
  end.
