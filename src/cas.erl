-module(cas).

-export([init/2, read/2, write/3, delete/1, delete/2, match/3]).

%%-----------------------------------------------------------------------------
init(Tab, Dir) ->
    case ets:info(Tab, size) of
        undefined ->
            ets:new(Tab, [named_table, ordered_set]),
            ets:insert(Tab, {{meta, data_dir}, Dir}),
            ok = filelib:ensure_dir(data_file(Tab, dummy));
        _ ->
            {error, exists}
    end.

read(Tab, Key) ->
    case ets:lookup(Tab, {data, Key}) of
        [] -> [];
        [{_, _, [Val]}] -> [{Key, Val}]
    end.

write(Tab, Key, Val) ->
    try
        OldVal = lock(Tab, Key),
        case persist(Tab, Key, Val) of
            ok ->
                unlock(Tab, Key, OldVal, [Val]);
            {error, Err} ->
                unlock(Tab, Key, OldVal, OldVal),
                error({error_persisting, {Tab, Key, Err}})
        end
    catch
        throw:Abort -> {aborted, Abort}
    end.

delete(Tab) ->
    Source = data_dir(Tab),
    Dest = Source++"_",
    file:rename(Source, Dest),
    catch ets:delete(Tab),
    spawn(fun() -> rmrf(Dest) end),
    ok.

delete(Tab, Key) ->
    try lock(Tab, Key) of
        [Val] ->
            file:delete(data_file(Tab, Key)),
            ets:delete(Tab, {data, Key}),
            [{Key, Val}];
        [] ->
            []
    catch
        throw:Abort -> {aborted, Abort}
    end.

match(Tab, K, V) ->
    case K of
        '_' ->
            ets:foldl(mk_matchf(V), [], Tab);
        _ ->
            Matches = ets:select(Tab, [{{{data, K}, '_', '_'}, [], ['$_']}]),
            lists:foldl(mk_matchf(V), [], Matches)
    end.
%%----------------------------------------------------------------------------
lock(Tab, Key) ->
    case ets:lookup(Tab, {data, Key}) of
        [] -> lock_new(Tab, Key);
        [{_, unlocked, Old}] -> lock_old(Tab, Key, Old);
        [{_, locked, _}] -> throw(locked);
        Err -> error({error_lookup,Tab, Key, Err})
    end.

lock_new(Tab, Key) ->
    case ets:insert_new(Tab, {{data, Key}, locked, []}) of
        true -> [];
        false -> throw(collision)
    end.

lock_old(Tab, Key, Val) ->
    case cas(Tab, {{data, Key}, unlocked, Val}, {{data, Key}, locked, Val}) of
        true -> Val;
        false -> throw(collision)
    end.

unlock(Tab, Key, Old, New) ->
    case cas(Tab, {{data, Key}, locked, Old}, {{data, Key}, unlocked, New}) of
        true -> ok;
        false -> error({error_unlocking, {Tab, Key}})
    end.

cas(Tab, Old, New) ->
    case ets:select_replace(Tab, [{Old, [], [{const, New}]}]) of
        1 -> true;
        0 -> false;
        R -> error({error_many_rows, {Tab, element(1, Old), R}})
    end.

mk_matchf(V) ->
    fun({{data, Key}, _, [Val]}, Acc) ->
            try
                match(V, Val),
                [{Key, Val}|Acc]
            catch
                _:_ -> Acc
            end;
       (_, Acc) ->
            Acc
    end.

match('_', _) ->
    true;
match(V, V) ->
    true;
match(A, B) when is_map(A), is_map(B) ->
    maps:filter(mk_mapf(B), A);
match(A, B) when is_list(A), is_list(B) ->
    lists:zipwith(fun(Ea, Eb) -> match(Ea, Eb) end, A, B);
match(A, B) when is_tuple(A), is_tuple(B) ->
    match(tuple_to_list(A), tuple_to_list(B)).

mk_mapf(M) ->
    fun(K, V) -> match(V, maps:get(K, M)) end.

persist(Tab, Key, Val) ->
    file:write_file(data_file(Tab, Key), term_to_binary({Key, Val})).

rmrf(F) ->
    case file:list_dir(F) of
        {ok, Fs} ->
            lists:foreach(fun rmrf/1, [filename:join(F,E) || E <- Fs]),
            file:del_dir(F);
        {error, enotdir} ->
            file:delete(F);
        {error, _} ->
            ok
    end.

data_file(Tab, Key) ->
    Name = integer_to_list(erlang:phash2(Key, 4294967296)),
    filename:join(data_dir(Tab), Name).

data_dir(Tab) ->
    [{_, Dir}] = ets:lookup(Tab, {meta, data_dir}),
    filename:join(Dir, Tab).

%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
  {"basic happy testing.",
   {foreach,
    fun start/0,
    fun stop/1,
    [fun t_match/1,
     fun t_lookup/1,
     fun t_delete/1
    ]
   }}.

start() ->
    cas:init(foo, "/tmp/cas"),
    cas:write(foo, {a,b}, #{a => "A",b => "B"}),
    cas:write(foo, {a,c}, #{a => "A",b => "C"}).

stop(_) ->
    cas:delete(foo).

t_match(_) ->
    [?_assertMatch([{{a,_},#{}},{{a,_},#{}}],
                   cas:match(foo, '_', #{a => "A"}))].

t_lookup(_) ->
    [?_assertMatch([{{a,b}, #{}}], cas:read(foo, {a,b}))].

t_delete(_) ->
    cas:delete(foo,{a,b}),
    [?_assertMatch([], cas:read(foo, {a,b}))].

-endif.
