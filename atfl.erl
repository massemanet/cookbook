%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  8 Oct 2012 by mats cronqvist <masse@klarna.com>

%% @doc
%% A tiny filter language. 
%% Things like these;
%% atfl:run(atfl:compile("ets:inet_db.3"),hostname).
%% atfl:run(atfl:compile("erlang:process_info.current_function.1"),self()).
%%
%% produce the expected results.
%% The basic idea is that the second arg of run/2 gets passed through a series
%% of mappings, expressed by the argument to compile/1.
%% @end

-module('atfl').
-author('mats cronqvist').
-export([compile/1,run/2]).

run([],A) -> A;
run([I|Is],A) -> run(Is,I(A)).

compile(Str) ->
  Types = types(),
  [do(Types,I) || I <- string:tokens(Str,".")].

do(Types,I) ->
  case first(Types,I) of
    {M,F} -> wrap2(M,F);
    X     -> wrap1(X)
  end.

wrap1(X) ->
  fun(M) ->
      case M of
        ""                 -> "";
        [{_,_}|_]          -> proplists:get_value(X,M);
        [_|_]              -> lists:nth(X,M);
        _ when is_tuple(M) -> element(X,M)
      end
  end.

wrap2(ets,T) ->
  fun(K)->
      try element(2,hd(ets:lookup(T,K)))
      catch _:_ -> ""
      end
  end;
wrap2(M,F) ->
  fun(V)->
      try M:F(V)
      catch _:_ -> ""
      end
  end.

first([T|Ts],I) ->
  try T(I)
  catch _:_ -> first(Ts,I)
  end.

types() ->
  [fun(I) -> [M,F] = string:tokens(I,":"),{list_to_atom(M),list_to_atom(F)}end,
   fun(I) -> list_to_integer(I)end,
   fun(I) -> list_to_atom(I)end].
