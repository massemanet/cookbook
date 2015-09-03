%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  3 Sep 2015 by  <masse@klarna.com>

%% @doc
%% @end

-module(longest_palindrome).
-export([go/0,go/1]).

go() ->
  [go(X) || X <- ["abcdefgfedfrg","abcdefg"].

go(X) ->
  [].
