%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 20 Jan 2014 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('boilerplate').
-author('mats cronqvist').

%% the API
-export([start/0,stop/0,state/0,unlink/0]).

%% for application supervisor
-export([start_link/0]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1,terminate/2,code_change/3,
         handle_call/3,handle_cast/2,handle_info/2]).

%% declare the state
-record(b,{uno=1,due=[1,2]}).
-record(state,{a={},b=#b{}}).

%% add all records here, to kludge around the record kludge.
rec_info(state) -> record_info(fields,state);
rec_info(b)     -> record_info(fields,b);
rec_info(_)     -> [].

%% the API
start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

unlink() ->
  gen_server:call(?MODULE,unlink).

state() ->
  gen_server:call(?MODULE,state).

%% for application supervisor
start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% gen_server callbacks
init(_) ->
  {ok,#state{}}.

terminate(_Reason,_State) ->
  ok.

code_change(_OldVsn,State,_Extra) ->
  {ok,State}.

handle_call(state,_From,State) ->
  {reply,expand_recs(State),State};
handle_call(unlink,_From,State) ->
  {links,Links} = process_info(self(),links),
  lists:foreach(fun unlink/1,Links),
  {reply,ok,State};
handle_call(What,_From,State) ->
  {reply,What,State}.

handle_cast(_What,State) ->
  {noreply,State}.

handle_info(_What,State) ->
  {noreply,State}.

%% utilities
expand_recs(List) when is_list(List) ->
  [expand_recs(I) || I <- List];
expand_recs(Tup) when is_tuple(Tup) ->
  case tuple_size(Tup) of
    L when L < 1 -> Tup;
    L ->
      try Fields = rec_info(element(1,Tup)),
          L = length(Fields)+1,
          lists:zip(Fields,expand_recs(tl(tuple_to_list(Tup))))
      catch _:_ ->
          list_to_tuple(expand_recs(tuple_to_list(Tup)))
      end
  end;
expand_recs(Term) ->
  Term.
