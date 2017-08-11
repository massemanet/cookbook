-module(cas).

-export([init/1, read/2, write/3]).

init(Tab) ->
    ok = filelib:ensure_dir(data_file(Tab, dummy)),
    ets:new(Tab, [named_table, ordered_set]).

read(Tab, Key) ->
    case ets:lookup(Tab, Key) of
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


lock(Tab, Key) ->
    case ets:lookup(Tab, Key) of
        [] -> lock_new(Tab, Key);
        [{Key, unlocked, Old}] -> lock_old(Tab, Key, Old);
        [{Key, locked, _}] -> throw(locked);
        Err -> error({error_lookup,Tab, Key, Err})
    end.

lock_new(Tab, Key) ->
    case ets:insert_new(Tab, {Key, locked, []}) of
        true -> [];
        false -> throw(collision)
    end.

lock_old(Tab, Key, Val) ->
    case cas(Tab, {Key, unlocked, Val}, {Key, locked, Val}) of
        true -> Val;
        false -> throw(collision)
    end.

unlock(Tab, Key, Old, New) ->
    case cas(Tab, {Key, locked, Old}, {Key, unlocked, New}) of
        true -> ok;
        false -> error({error_unlocking, {Tab, Key}})
    end.

cas(Tab, Old, New) ->
    case ets:select_replace(Tab, [{Old, [], [{const, New}]}]) of
        1 -> true;
        0 -> false;
        R -> error({error_many_rows, {Tab, element(1, Old), R}})
    end.

persist(Tab, Key, Val) ->
    file:write_file(data_file(Tab, Key), term_to_binary({Key, Val})).

data_file(Tab, Key) ->
    filename:join(data_dir(Tab), Key).

data_dir(Tab) ->
    filename:join("/tmp", Tab).
