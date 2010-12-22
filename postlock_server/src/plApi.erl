%%%-------------------------------------------------------------------
%%% File    : plApi.erl
%%% Author  : Peter Neumark 
%%% Description : Server-side API for building postlock applications
%%%     Currently used to initiate websocket connection with client.
%%%-------------------------------------------------------------------
-module(plApi).
-export([connect_websocket/2]).
-include("yaws_api.hrl").
%%--------------------------------------------------------------------
%% Function: connect_websocket(A, Sessionid) -> YAWS response
%% Description: Call this function from a .yaws file to upgrade the 
%% HTTP connection to a websocket connection and connect to a client
%% to a sync server (and state server).
%% A is the yaws args variable, sessionid is the numeric session id
%% which the client will connect to.
%%--------------------------------------------------------------------
connect_websocket(ArgHeaders, SessionId) ->
    case get_upgrade_header(ArgHeaders) of 
	undefined ->
	    {content, "text/plain", "You're not a web sockets client! Go away!"};
	"WebSocket" ->
	    WebSocketOwner = spawn(fun() -> websocket_owner(SessionId) end),
	    {websocket, WebSocketOwner, passive};
    _Any ->
         {content, "text/plain", "Something wierd happened!"}
    end.

websocket_owner(SessionId) ->
    receive
	{ok, WebSocket} ->
	    %% This is how we read messages (plural!!) from websockets on passive mode
	    case yaws_api:websocket_receive(WebSocket) of
		{error,closed} ->
		    io:format("The websocket got disconnected right from the start. "
			      "This wasn't supposed to happen!!~n");
		{ok, Messages} ->
		    case Messages of
			[<<"client-connected">>] ->
			    yaws_api:websocket_setopts(WebSocket, [{active, true}]),
			    {SyncServer, ClientId} = gen_server:call(plRegistry, {new_client, SessionId, self()}),
                % We want to exit if the sync server dies.
                erlang:link(SyncServer),
                error_logger:info_report(["got sync server for client", ClientId]),
			    listen_loop(WebSocket, SyncServer);
			Other ->
			    io:format("websocket_owner got: ~p. Terminating~n", [Other])
		    end
	    end;
	_ -> ok
    end.

listen_loop(WebSocket, SyncServer) ->
    receive
	{tcp, WebSocket, DataFrame} ->
        % Try to decode all JSON messages 
        % received through the websocket.
        Lin = lists:map(fun(X) -> json:decode_string (erlang:binary_to_list(X)) end, 
            yaws_websockets:unframe_all(DataFrame, [])),
        [gen_fsm:send_event(SyncServer, {client_message, Msg}) || Msg <- Lin],
        listen_loop(WebSocket, SyncServer);
	{tcp_closed, WebSocket} ->
        gen_fsm:send_all_state_event(SyncServer, websocket_closed),
	    io:format("Websocket closed. Terminating listen_loop...~n");
    {send, ToBeSent} ->
        yaws_api:websocket_send(WebSocket, ToBeSent),
        listen_loop(WebSocket, SyncServer);
	Any ->
	    io:format("listen_loop received msg:~p~n", [Any]),
	    listen_loop(WebSocket, SyncServer)
    end.

get_upgrade_header(#headers{other=L}) ->
    lists:foldl(fun({http_header,_,K0,_,V}, undefined) ->
                        K = case is_atom(K0) of
                                true ->
                                    atom_to_list(K0);
                                false ->
                                    K0
                            end,
                        case string:to_lower(K) of
                            "upgrade" ->
                                V;
                            _ ->
                                undefined
                        end;
                   (_, Acc) ->
                        Acc
                end, undefined, L).

