%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 31 Jan 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% Short Unique Koncise Reference
%% @end

-module('sukr').
-author('mats cronqvist').
-export([compose/2,decompose/1]).

%% construct a sukr
%% C is an integer; Counter
%% I is an integer; machine ID
compose(C,I) ->
  {L,CL,IL} = lengths(C,I),
  PC = parity(C),
  PI = parity(I),
  <<SUKR:L>> = <<PC:1,PI:1,1:1,C:CL,I:IL>>,
  tobase30(SUKR).

%% validate and extract Counter and machine ID from a SUKR.
%% (worthless exept for testing.)
%% SUKR is a string.
decompose(SUKR) ->
  {L,CL,IL} = lengths(length(SUKR)),
  S = frombase30(SUKR),
  <<PC:1,PI:1,1:1,C:CL,I:IL>> = <<S:L>>,
  PC = parity(C),
  PI = parity(I),
  {C,I}.

%% encodes integer to base 30 string
tobase30(X) ->
  [tr_out(C) || C <- lists:flatten(io_lib:fwrite("~.30B",[X]))].

%% decodes base 30 string to integer
frombase30(X) ->
  {ok,[S],[]} = io_lib:fread("~#","30#"++[tr_in(C) || C <- X]), S.

tr_out($A) -> $V;
tr_out($E) -> $W;
tr_out($I) -> $X;
tr_out($O) -> $Z;
tr_out(C)  -> C.

tr_in($V) -> $A;
tr_in($W) -> $E;
tr_in($X) -> $I;
tr_in($Z) -> $O;
tr_in(C)  -> C.

%% C is value of counter; I is value of machineID
%% returns length in bits of {total, counter, machineID}
lengths(C,I) -> lengths(C,I,4).
lengths(0,0,N) -> lengths(N-1);
lengths(C,0,11)-> exit({c_too_big,C});
lengths(0,I,11)-> exit({i_too_big,I});
lengths(C,I,N) ->
  {_,CL,IL} = lengths(N),
  lengths(case C < (1 bsl CL) of true -> 0; false -> C end,
          case I < (1 bsl IL) of true -> 0; false -> I end,
          N+1).

%% N is number of chars in SUKR
%% returns length in bits of; {total, counter, machineID}
lengths(N) ->
  {5*N-1, 4*N-3, N-1}.

%% calculates parity of an integer
%% returns 1 || 0
parity(C)   -> parity(C,false).
parity(0,true) -> 1;
parity(0,false)-> 0;
parity(C,P) -> parity((C band (C-1)),not P).
