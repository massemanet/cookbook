%% -*- mode: erlang; erlang-indent-level: 2 -*-
%%% Created :  6 Dec 2013 by mats cronqvist <masse@klarna.com>

%% @doc
%% @end

-module('sax').
-author('mats cronqvist').
-export([test/0, test/1, test/2]).

test() ->
  test(fun(Ev,St) -> [Ev|St] end).
test(Fun) ->
  FN = "/usr/share/doc/libxml2-*/html/examples.xml",
  {ok,XML} = file:read_file(filelib:wildcard(FN)),
  test(Fun,XML).
test(Fun,RawXML) ->
  {ok,O,Rest} = xmerl_sax_parser:stream(RawXML,[{event_fun,sax_if(Fun)}]),
  case Rest of
    <<>> -> O;
    [] -> O;
    _ -> exit({rest,Rest})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SAX funs
-define(BEG(El,Attrs), {startElement,_,El,_,Attrs}).
-define(END(El),       {endElement,_,El,_}).
-define(CHS(Cs),       {characters,Cs}).
-define(ATT(Id,Val),   {attribute,Id,_,_,Val}).
sax_if(Fun) ->
  fun(startDocument,_,_)   -> Fun(startDoc,[]);
     (?BEG(El,As),_,State) -> Fun({startEl,El,[{Id,V}||?ATT(Id,V)<-As]},State);
     (?END(El),_,State)    -> Fun({endEl,El},State);
     (?CHS(Cs),_,State)    -> Fun({chs,Cs},State);
     (endDocument,_,State) -> Fun(endDoc,State);
     (_,_,State)           -> State
  end.
