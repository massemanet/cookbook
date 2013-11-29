%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  7 Feb 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% read a file in /etc/passwd format, search for and print duplicate uid fields
%% @end

-module('goo').
-author('mats cronqvist').
-export([main/0]).

main() ->
  main("/etc/passwd").
main(FName) ->
  {ok,FD} = file:open(FName,[read]),
  UidNames = line(FD,file:read_line(FD),[]),
  print(check(lists:sort(UidNames),[])).

line(_,eof,Acc)       -> Acc;
line(FD,{ok,Str},Acc) -> line(FD,file:read_line(FD),acc_app(Str,Acc)).

acc_app(Str,Acc) ->
  case string:tokens(Str,":") of
    [Name,_,UID|_] -> [{UID,Name}|Acc];
    _              -> Acc
  end.

check([{UID,Name}|R], [{UID,N,OtherName}|Acc]) ->
  check(R,[{UID,N+1,OtherName++":"++Name}|Acc]);
check([{UID,Name}|R],Acc) ->
  check(R,[{UID,1,Name}|Acc]);
check([],Acc) ->
  Acc.

print([{_,1,_}|R]) ->
  print(R);
print([{UID,_,Names}|R]) ->
  erlang:display({UID,Names}),
  print(R);
print([]) ->
  ok.
