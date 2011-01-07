%%%-------------------------------------------------------------------
%%% File    : plMessage.erl
%%% Author  : Peter Neumark
%%% Description : 
%%% Message reading/writing functions for postlock.
%%% Created :  4 Jan 2011 by Peter Neumark
%%%-------------------------------------------------------------------
-module(plMessage).
-export([parse_raw_message/1,
         make_message/1,
         make_message/3,
         json_get_value/2,
         json_get_value/3]).

-include("plMessage.hrl").

make_message(Type) ->
    make_message(Type, [], []).

make_message(Type, Header, Body) ->
    to_string({struct,[
        {"type", Type},
        {"header", Header},
        {"body", Body}]}).

parse_raw_message(Msg) when is_binary(Msg) ->
    parse_raw_message(erlang:binary_to_list(Msg));
parse_raw_message(Msg) ->
    try
        {ok, Json} = json:decode_string(Msg),
        {ok, Type} = json_get_value([type],Json),
        {_, Header} = json_get_value([header],Json, []),
        {_, Body} = json_get_value([body],Json, []),
        #postlock_message{
            type=Type,
            header=Header,
            body=Body
        }
    catch error:{badmatch, _} -> {bad_message, Msg}
    end.

json_get_value(KeyList, JsonMessage) ->
    try
        {ok, json_get_value_1(KeyList, JsonMessage)}
    catch
        'exit':{struct_no_key, BadKey} -> {error, BadKey}
    end.

json_get_value(KeyList, JsonMessage, DefaultValue) ->
    try
        {ok, json_get_value_1(KeyList, JsonMessage)}
    catch
        'exit':{struct_no_key, _Key} -> {default, DefaultValue}
    end.
%%%-------------------------------------------------------------------
%%% internal functions
%%%-------------------------------------------------------------------
json_get_value_1([], Value) -> Value;
json_get_value_1([Key|KeyList], Obj) ->
    json_get_value_1(KeyList, json:obj_fetch(Key, Obj)).

to_string(Struct) ->
    lists:flatten(json:encode(Struct)).

