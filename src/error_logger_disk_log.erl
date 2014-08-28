%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created : 28 Aug 2014 by  <masse@cronqvi.st>

%% @doc
%% error_logger callback. write to a disk_log.
%% start like this; 
%% error_logger:add_report_handler(error_logger_disk_log,Opts).
%%  Opts: [{K,V}]
%%    where K: 'dir' (defaults to "/tmp")
%%             'filesize' (defaults to 1024*1024)
%%             'filecount' (defaults to 16)
%% @end
-module(error_logger_disk_log).
-behaviour(gen_event).

-export([init/1,terminate/2,code_change/3,
	 handle_event/2, handle_call/2, handle_info/2]).

-include_lib("kernel/include/file.hrl").

state(Opts) ->
  [{name      ,errors},
   {dir       ,proplists:get_value(dir,Opts,"/tmp")},
   {filesize  ,proplists:get_value(filesize,Opts,1024*1024)},
   {filecount ,proplists:get_value(filecount,Opts,16)}].

handle_event(What,State) ->
  out(What,State),
  {ok,State}.

handle_info(What,State) ->
  out(What,State),
  {ok,State}.

handle_call(state,State) ->
  {ok,State,State};
handle_call(What,State) ->
  {ok,{what,What},State}.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

terminate(_Reason,State) ->
  Name = proplists:get_value(name,State),
  disk_log:close(Name),
  State.

init(Opts) ->
  State = state(Opts),
  try
    assert_logdir(State),
    assert_open(State),
    {ok,State}
  catch
    throw:R -> {error,R}
  end.

assert_open(State) ->
  Name = proplists:get_value(name,State),
  Dir  = proplists:get_value(dir,State),
  Filesize  = proplists:get_value(filesize,State),
  Filecount  = proplists:get_value(filecount,State),
  Attributes = [{name,Name},
                {file,filename:join([Dir,Name])},
                {type,wrap},
                {format,external},
                {size,{Filesize,Filecount}}],
  case disk_log:open(Attributes) of
    {error,R} -> throw({cannot_open_logfile,R,Attributes});
    {ok,_}    -> ok
  end.

assert_logdir(Attributes) ->
  Dir      = proplists:get_value(dir,Attributes),
  Name     = proplists:get_value(name,Attributes),
  Filename = filename:join([Dir,Name]),
  case filelib:ensure_dir(Filename) of
    ok -> ok;
    {error,R} -> throw({cannot_find_logdir,R,Filename})
  end,
  try
    {ok,#file_info{type=Type,access=Access}} = file:read_file_info(Dir),
    read_write = Access,
    directory = Type
  catch
    _:R1 -> throw({cannot_write_to_logdir,R1,Filename})
  end.

out(What,State) ->
  Name = proplists:get_value(name,State),
  Out = list_to_binary(io_lib:format("~n====~n~p~n",[What])),
  disk_log:balog(Name,Out).
