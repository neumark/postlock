%%%-------------------------------------------------------------------
%%% File    : plDebug.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description :
%%%   Module for debugging postlock, primarily through seq_trace.
%%%   For a description of seq_trace, see 
%%%   http://www.erlang.org/doc/man/seq_trace.html which is the
%%%   source of much of this code.
%%% Created :  21 Feb 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plDebug).
-compile(export_all).
-define(TRACE_LABEL, 17).

trace_this() ->
    seq_trace:set_token(label,?TRACE_LABEL),
    seq_trace:set_token('receive',true),
    seq_trace:set_token(send,true),
    seq_trace:set_token(print,true).

print(Msg) ->
    seq_trace:print(?TRACE_LABEL, Msg).

tracer() -> % lifted from http://www.erlang.org/doc/man/seq_trace.html
    receive
        {seq_trace,Label,TraceInfo} ->
           print_trace(Label,TraceInfo,false);
        {seq_trace,Label,TraceInfo,Ts} ->
           print_trace(Label,TraceInfo,Ts);
        _Other -> ignore
    end,
    tracer().        

print_trace(Label,TraceInfo,false) ->
    io:format("~p:",[Label]),
    print_trace(TraceInfo);
print_trace(Label,TraceInfo,Ts) ->
    io:format("~p ~p:",[Label,Ts]),
    print_trace(TraceInfo).
print_trace({print,Serial,From,_,Info}) ->
    io:format("~p Info ~p WITH~n~p~n", [From,Serial,Info]);
print_trace({'receive',Serial,From,To,Message}) ->
    io:format("~p Received ~p FROM ~p WITH~n~p~n", 
              [To,Serial,From,Message]);
print_trace({send,Serial,From,To,Message}) ->
    io:format("~p Sent ~p TO ~p WITH~n~p~n", 
              [From,Serial,To,Message]).

start() ->
    Pid = spawn(?MODULE, tracer, []),
    seq_trace:set_system_tracer(Pid),
    Pid.
