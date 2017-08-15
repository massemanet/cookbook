-module(cas).

-export([init/2, read/2, write/3, match/3]).

%%-----------------------------------------------------------------------------
init(Tab, Dir) ->
    ets:new(Tab, [named_table, ordered_set]),
    ets:insert({{meta, data_dir}, Dir}),
    ok = filelib:ensure_dir(data_file(Tab, dummy)).

read(Tab, Key) ->
    case ets:lookup(Tab, {data, Key}) of
        [] -> [];
        [{_, _, Val}] -> Val
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
    fun({_, _, []}, Acc) -> Acc;
       ({{data, Key}, _, [Val]}, Acc) ->
            case match(V, Val) of
                true -> [{Key, Val}|Acc];
                false -> Acc
            end
    end.

match(V, V) ->
    ok;
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

data_file(Tab, Key) ->
    [{_, Dir}] = ets:lookup(Tab, {meta, data_dir}),
    filename:join(Dir, Key).
