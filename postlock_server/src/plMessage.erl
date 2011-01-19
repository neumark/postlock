%%%-------------------------------------------------------------------
%%% File    : plMessage.erl
%%% Author  : Peter Neumark
%%% Description : 
%%% Message reading/writing functions for postlock.
%%% Created :  4 Jan 2011 by Peter Neumark
%%%-------------------------------------------------------------------
-module(plMessage).
-export([parse_raw_message/1,
         make_error/1,
         make_message/1,
         make_message/3,
         transaction_new/0,
         make_transformation/3,
         transaction_add_transformation/2,
         transaction_add_ack/2,
         transaction_add_nack/2,
         transaction_set_id/2,
         transaction_set_client/2,
         transaction_set_user/2,
         transaction_serialize/1,
         json_get_value/2,
         json_get_value/3]).

-include("plMessage.hrl").
-include("plState.hrl").

%%% ----------------------------------------------------------
%%% postlock_transaction - contains a group of transformations
%%% received by the sync server from a client.
%%% ----------------------------------------------------------
-record(pl_server_transaction, {
    id,                     % integer (key)
    user,                   % user who submitted the transaction
    client,                 % client which submitted the transaction
    acks = [],               % a list of ACKs sent by the client
                            % or server
    nacks = [],              % a list of NACKs sent by the server
    transformations = [],   % list of transformations
    created  = now()        % timestamp of transaction
}).

make_error(Reason) ->
    make_message(
        "error",
        [],
        {struct, [{"reason", Reason}]}
    ).

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
%% == Transformations ==
make_transformation(Oid, Command, Parameters) ->
    {struct,[ 
        {"oid", Oid},
        {"command", Command},
        {"parameters", Parameters}
    ]}.

transformation_to_json(#postlock_transformation{
    oid=Oid,
    cmd=Command,
    parameters=Parameters}) ->
    make_transformation(Oid, Command, Parameters).
%% == Transaction Handling ==

%% @doc Creates an empty transaction which can be filled later. 
transaction_new() -> #pl_server_transaction{}.

transaction_add_transformation(Transformation ,T) ->
    T#pl_server_transaction{transformations = [Transformation|T#pl_server_transaction.transformations]}.

transaction_add_ack(Ack, T) ->
    T#pl_server_transaction{acks = [Ack|T#pl_server_transaction.acks]}.

transaction_add_nack(Nack, T) ->
    T#pl_server_transaction{nacks = [Nack|T#pl_server_transaction.nacks]}.

transaction_set_id(Id, T) ->
    T#pl_server_transaction{id = Id}.

transaction_set_client(Client, T) ->
    T#pl_server_transaction{client = Client}.

transaction_set_user(User, T) ->
    T#pl_server_transaction{user = User}.


%% @doc Serializes the transaction into a string which can be
%% sent over a websocket to the client.
transaction_serialize(T) ->
    make_message(
        "transaction",                          % type
        {struct,  [                             % header 
            {"id", T#pl_server_transaction.id},
            {"results", {struct,
                    [{"ack", {array, lists:reverse(T#pl_server_transaction.acks)}},
                     {"nack", {array, lists:reverse(T#pl_server_transaction.nacks)}}
                      ]}}]},
        {struct, [{"transformations", {array, 
            [transformation_to_json(X) || X <- lists:reverse(T#pl_server_transaction.transformations)]}
            }]}     % body
     ).

%% == internal functions ==
json_get_value_1([], Value) -> Value;
json_get_value_1([Key|KeyList], Obj) ->
    json_get_value_1(KeyList, json:obj_fetch(Key, Obj)).

to_string(Struct) ->
    lists:flatten(json:encode(Struct)).



