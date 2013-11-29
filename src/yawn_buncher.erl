%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 10 May 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% uses yawn and buncher to aggregate data into bunches.
%% As an example, try this;
%%  buncher:start().
%%  yawn_buncher:start(oo).
%%  buncher:source().
%% and do e.g. this;
%%  curl http://localhost:6666/bunch
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

handler(http,?HttpEoh(),S)               -> {keep,S};
handler(http,?HttpError(Val),_)          -> {close,flat(Val)};
handler(http,?HttpHeader(_,_),S)         -> {keep,S};
handler(http,?HttpRequest(Meth,Uri,_),S) ->
  case {Meth,Uri} of
    {'GET',{abs_path,<<"/bunch">>}} -> buncher ! {attach,self()},{keep,S};
    _ -> {close,flat({Meth,Uri})}
  end;
handler(msg,Data,_) ->
  {close,flat(Data)}.

flat(Term) -> lists:flatten(io_lib:fwrite("~p",[Term])).
