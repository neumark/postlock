%%%-------------------------------------------------------------------
%%% File    : plApi.erl
%%% Author  : Peter Neumark 
%%% Description : Server-side API for building postlock applications
%%%     Currently used to initiate websocket connection with client.
%%%-------------------------------------------------------------------
-module(plApi).
-export([connect_websocket/2]).
%%--------------------------------------------------------------------
%% Function: connect_websocket(A, Sessionid) -> YAWS response
%% Description: Call this function from a .yaws file to upgrade the 
%% HTTP connection to a websocket connection and connect to a client
%% to a sync server (and state server).
%% A is the yaws args variable, sessionid is the numeric session id
%% which the client will connect to.
%%--------------------------------------------------------------------
connect_websocket(ArgHeaders, SessionId) ->
    % Request sync server for new client from plRegistry.
    case gen_server:call(plRegistry, {new_client, SessionId, {websocket, ArgHeaders}}) of
        {error, Reason} ->
            io:format("Error initializing plGateway server: ~p~n", [Reason]),
            {content, "text/plain", "Your browser seems to lack websockets support."};
        {ok, SyncServer, _ClientId} ->
            {websocket, gen_fsm:sync_send_event(SyncServer, {get_websocket_owner}), passive}
    end.
