%%%-------------------------------------------------------------------
%%% File    : plMessage.erl
%%% Author  : Peter Neumark
%%% Description : 
%%% Message parsing utility functions for postlock
%%% Created :  4 Jan 2011 by Peter Neumark
%%%-------------------------------------------------------------------
-module(plMessage).
-export([
    json_get_value/2,
    json_get_value/3,
    finalize/1]).

-include("plMessage.hrl").
-include("plState.hrl").

%%% ----------------------------------------------------------
%%% postlock_transaction - contains a group of transformations
%%% received by the sync server from a client.
%%% ----------------------------------------------------------
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

finalize(Struct) ->
    json:encode(Struct).

%% == internal functions ==
json_get_value_1([], Value) -> Value;
json_get_value_1([Key|KeyList], Obj) ->
    json_get_value_1(KeyList, json:obj_fetch(Key, Obj)).


