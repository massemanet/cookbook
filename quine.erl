-module(cb_quine).
-export([c/0]).
c() ->
  io:fwrite("-module(cb_quine).~n-export([c/0]).~nc() ->~n  ~s~nd() ->~n  ~w.~n",[atom_to_list(d()),d()]).
d() ->
  'io:fwrite("-module(cb_quine).~n-export([c/0]).~nc() ->~n  ~s~nd() ->~n  ~w.~n",[atom_to_list(d()),d()]).'.
