-module(quine).
-export([c/0]).
c() ->
  Fmt = "-module(quine).~n-export([c/0]).~nc() ->~n  ~s~nd() ->~n  ~w.~n",
  io:fwrite(Fmt,[atom_to_list(d()),d()]).
d() ->
  'io:fwrite("-module(quine).~n-export([c/0]).~nc() ->~n  ~s~nd() ->~n  ~w.~n",[atom_to_list(d()),d()]).'.
