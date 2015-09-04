%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  3 Sep 2015 by  <masse@klarna.com>

%% @doc
%% @end

-module(longest_palindrome).
-export([go/0,go/1]).

go() ->
  [go(X) || X <- ["abcdefgfedfrg","abcdefg"]].

go(X) ->
  try [test(string:substr(X,S,L)) || L <- lengths(X), S <- starts(L,X)]
  catch A -> A
  end.

lengths(X) -> lists:reverse(lists:seq(1,length(X))).

starts(L,X) -> lists:seq(1,length(X)-L+1).

test(X) ->
  case X =:= lists:reverse(X) of
    true -> throw(X);
    false-> ok
  end.
