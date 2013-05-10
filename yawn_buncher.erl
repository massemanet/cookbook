%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 May 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('yawn_buncher').
-author('mats cronqvist').
-export([start/1,stop/1]).

stop(Name) -> yawn:stop(Name).

start(Name) -> yawn:start(Name,[{handler,fun handler/3}]).

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

flat(Term) -> lists:flatten(io_lib:fwrite("~p",[Term])).
