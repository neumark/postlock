%%%-------------------------------------------------------------------
%%% File    : plSync.erl
%%% Author  : Peter Neumark
%%% Description : 
%%% A plSync process handles the dialog with the each client.
%%% YAWS gives us the JSON-encoded message from the client, then
%%% it's up to plSync to:
%%% - Handle initialization of newly connected clients
%%% - Read ACKs for server transactions from the message, and
%%%   update the server's understanding of the client's state.
%%% - Forward client transactions to the state server.
%%% - Push modifications coming from state-server to the client
%%%   through websockets.
%%%
%%% Created :  7 Dec 2010 by Peter Neumark
%%%-------------------------------------------------------------------
-module(plSync).
-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([
        % state functions
        idle/2, idle/3,
        auth/2, auth/3,
        initial_copy/2, initial_copy/3,
        connected/2, connected/3,
        % other functions
        connect_websocket/1,
        websocket_owner/1,
        % standard gen_fsm exports
        handle_event/3, handle_sync_event/4, handle_info/3, terminate/3,
        code_change/4, init/1]).

-include("plState.hrl").
-include("plMessage.hrl").
-include("plError.hrl").
-include("yaws_api.hrl").

-record(state, {
          % The id of the client.
          client_id,
          % The postlock user represented by
          % the client
          user_id = "NA", %TODO: update auth-challege/response to set UID
          % low-level websocket data from YAWS
          websocket_data,
          % The PID of the process which owns the websocket, used to
          % push data to the client.
          websocket_owner,
          % PID of the state server, which we forward transactions to
          % and get updates from
          state_server,
          % transaction id counter
          transaction_id = 0
}).
-define(DEFAULT_TIMEOUT, 100000).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(ServerData) ->
    gen_fsm:start_link(?MODULE, ServerData, []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([StateServer, ClientId, {websocket, ArgsHeaders}]) ->
    process_flag(trap_exit, true),
    % Initialize websocket
    case connect_websocket(ArgsHeaders) of
        {ok, WebSocketOwner} ->
            {ok, idle, #state{
                   client_id = ClientId,
                   websocket_owner = WebSocketOwner,
                   state_server = StateServer
             }, ?DEFAULT_TIMEOUT}; 
        {error, Reason} ->
            {stop, {websocket_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% state: idle 
%%--------------------------------------------------------------------

idle({client_message, Msg}, State) when Msg#postlock_message.type == "client_connect" ->
    % put websocket in active mode
    State#state.websocket_owner ! {set_active_mode},
    % send server_connect message
    State#state.websocket_owner ! {send, plMessage:make_message("server_connect")},
    % send auth_challenge, which puts client in auth state.
    State#state.websocket_owner ! {send, plMessage:make_message("auth_challenge")},
    {next_state, auth, State, ?DEFAULT_TIMEOUT};

idle(Event, State) ->
    io:format("plSync:idle/2 got unexpected event ~p~n", [Event]),
    {next_state, idle, State}.

idle({get_websocket_owner}, _From, State) ->
    {reply, State#state.websocket_owner, idle, State};

idle(Event, _From, State) ->
    % NOT USED
    io:format("plSync:idle/3 got unexpected event ~p~n", [Event]),
    {reply, ok, idle, State}.

%%--------------------------------------------------------------------
%% state: auth
%%--------------------------------------------------------------------

auth({client_message, Msg}, State) when Msg#postlock_message.type == "auth_response" ->
    NumPublicObjects = gen_server:call(State#state.state_server, {get_num_public_objects}),
    OutMsg = plMessage:make_message("client_data", [], 
        {struct, [{"client_id", State#state.client_id}, {"num_objects", NumPublicObjects}]}),
    State#state.websocket_owner ! {send, OutMsg},
    {NewState, _CopiedSet} = do_initial_copy(State),
    % send ourselves a copy_finished message.
    gen_fsm:send_event(self(), {copy_finished}),
    {next_state, initial_copy, NewState};

auth(Event, State) ->
    io:format("plSync:auth/2 got unexpected event ~p~n", [Event]),
    {next_state, auth, State}.

auth(Event, _From, State) ->
    io:format("plSync:auth/3 got unexpected event ~p~n", [Event]),
    {reply, ok, auth, State}.

%%--------------------------------------------------------------------
%% state: initial_copy
%% At this point the transfer of object has already been completed.
%% We only need to transfer pending transactions, and send a
%% copy_finished message when done.
%%--------------------------------------------------------------------
initial_copy({copy_finished}, State) ->
    State#state.websocket_owner ! {send, plMessage:make_message("copy_finished")},
    {next_state, initial_copy, State};

initial_copy({client_message, Msg}, State) when Msg#postlock_message.type == "ack_copy" ->
    {next_state, connected, State};

initial_copy({client_message, Msg}, State) ->
    io:format("unpextected message in initial_copy from client: ~p~n", [Msg]),
    State#state.websocket_owner ! {send, plMessage:make_error(?BAD_SYNC_STATE_FOR_CLIENT_MESSAGE)},
    {next_state, initial_copy, State};

initial_copy({server_message, Msg}, State) ->
    io:format("TODO: relay server message to client ~p~n", [Msg]),
    {next_state, inital_copy, State};

initial_copy(Event, State) ->
    io:format("plSync:initial_copy/2 got unexpected event ~p~n", [Event]),
    {next_state, initial_copy, State}.

initial_copy(Event, _From, State) ->
    io:format("plSync:initial_copy/3 got unexpected event ~p~n", [Event]),
    {reply, ok, initial_copy, State}.

%%--------------------------------------------------------------------
%% state: connected
%%--------------------------------------------------------------------
connected({client_message, Msg}, State) when Msg#postlock_message.type == "transaction" ->
    T = 
    try
        {ok, Transformations} = plMessage:parse_transformations(Msg#postlock_message.body),
        % TODO: check that TID is the next expected TID from client!
        {ok, Tid} = plMessage:json_get_value([id], Msg#postlock_message.header),
        Transaction = #postlock_transaction{
            id=Tid,
            client_id=State#state.client_id,
            user_id=State#state.user_id
        },
        {ok, {Transaction, Transformations}}
    catch error:{badmatch, Reason} -> 
        {error, ["Error parsing transaction", State, Msg, Reason]}
    end,
    case T of
        {ok, Tdata} ->
            gen_server:cast(State#state.state_server, {client_transaction, Tdata});
        {error, E} ->
            error_logger:info_report(E),
            State#state.websocket_owner ! {send, 
                plMessage:make_error(?ERROR_PARSING_TRANSACTION, 
                json:obj_store(
                    "transaction_header",
                    Msg#postlock_message.header,
                    json:obj_new()))}
    end,
    {next_state, connected, State};

connected(Event, State) ->
    io:format("plSync:connected/2 got unexpected event ~p~n", [Event]),
    {next_state, connected, State}.

connected(Event, _From, State) ->
    io:format("plSync:connected/3 got unexpected event ~p~n", [Event]),
    {reply, ok, connected, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: 
%% do_initial_copy(State)-> NewState
%%                                     
%% Description: This function is called by init_client to replicate
%% the server state to the client. Since we don't lock state while
%% do_initial_copy is running, transactions from other clients will
%% be applied while do_inital_copy is running. As a result, the client
%% will get an inconsistent version of the global state.
%% Some of the transactions which occurred during do_initial_copy must
%% be run by the client later on.
%% We copy leaf nodes (objects of type data) first. Once all the
%% children of a list/dict are copied, we can copy the list also.
%% Note that objects can belong to several parents, so a naive
%% implementation could potentially be susceptible to infinite loops.
%%--------------------------------------------------------------------
do_initial_copy(State) ->
    do_initial_copy_1(
        [gen_server:call(State#state.state_server, {get_object, "0.0"})], 
        gb_trees:empty(), 
        State).

do_initial_copy_1([], CopiedSet, State) ->
    {State, CopiedSet};
do_initial_copy_1(AwaitingCopy = [NextObject|Rest], CopiedSet, State) ->
    case can_send_object(NextObject, CopiedSet, State#state.state_server) of
        already_sent ->
            do_initial_copy_1(Rest, CopiedSet, State);
        can_send ->
            NewState = send_object(NextObject, State),
            NewSet = gb_trees:enter(plObject:get_oid(NextObject), NextObject, CopiedSet),
            do_initial_copy_1(Rest, NewSet, NewState);
        Prerequisites ->
            % Add the prerequisites to the front of the queue.
            NewQueue = lists:foldl(fun(X,L) -> [X|L] end, AwaitingCopy, Prerequisites),
            do_initial_copy_1(NewQueue, CopiedSet, State)
    end.

send_object(Obj, State) ->
    % TODO Maybe I'll be smarter about this and not send
    % every object in its own transaction, but its 
    % quick and dirty time now! :)
    Tid = State#state.transaction_id,
    NewState = State#state{transaction_id = Tid + 1},
    Transformation = plObject:make_create_transformation(Obj),
    Transaction = lists:foldl(
        fun({Fun,Value},T) -> erlang:apply(plMessage,Fun,[Value,T]) end,
        % start with an empty transaction
        plMessage:transaction_new(),
        % apply the following list of commands
        [
            {transaction_set_id, Tid}, 
            {transaction_set_user, State#state.user_id}, 
            {transaction_set_client, State#state.client_id}, 
            {transaction_add_transformation, Transformation} 
        ]),
    % send Transaction to client
    State#state.websocket_owner ! {send, 
        plMessage:transaction_serialize(Transaction)},
    NewState.

can_send_object(Object, CopiedSet, StateServer) ->
    case gb_trees:is_defined(plObject:get_oid(Object), CopiedSet) of
        true ->
            already_sent;
        false ->
            Children = plObject:get_children(Object, StateServer),
            Uncopied = lists:filter(
                fun(Child) -> gb_trees:is_defined(Child, CopiedSet) end,
                Children),
            case Uncopied of 
                [] -> can_send;
                List -> List
            end
    end.

%%--------------------------------------------------------------------
%% Function: 
%% process_pending_transformation(Transformation, State)-> NewState
%%                                     
%% Description: This function is called by init_client to foward a
%% server transformation received during do_initial_copy. 
%% Only transformations not yet applied to the copied object are
%% forwarded.
%%--------------------------------------------------------------------
%process_pending_transformation(_Transformation, State) ->
%    % STUB -- do nothing for now.
%    State.
   
%%--------------------------------------------------------------------
%% Input message parsing functions
%%--------------------------------------------------------------------
%
%json_to_transaction(JsonMessage) ->
%    try
%        {ok, Id} = plMessage:json_get_value([header, id], JsonMessage),
%        {_, AckList} = plMessage:json_get_value([body, response, ack], JsonMessage, []),
%        {ok, {array, Tlist}} = plMessage:json_get_value([body, transformations], JsonMessage),
%        {ok, #postlock_transaction{
%            id=Id,
%            transformations = lists:map(fun interpret_transformation/1, Tlist)
%        }}
%    catch
%        _:_ -> {error, ["failed to parse transaction", JsonMessage]}
%    end.

%interpret_transformation(TMsg) ->
%    {ok, Cmd} = plMessage:json_get_value([command], TMsg),
%    {_, Oid} = plMessage:json_get_value([parameters, oid], TMsg, none),
%    {_, {struct, ParamList}} = plMessage:json_get_value([parameters], TMsg, {struct, []}),
%    #postlock_transformation{
%        cmd = Cmd,
%        oid = Oid,
%        parameters = ParamList
%    }.

%% == Websocket handling functions ==
connect_websocket(ArgsHeaders) ->
    case get_upgrade_header(ArgsHeaders) of 
	"WebSocket" ->
	    WebSocketOwner = spawn_link(?MODULE, websocket_owner, [self()]),
	    {ok, WebSocketOwner};
    BadHeader ->
        {error, {bad_header, BadHeader}}
    end.

websocket_owner(SyncServer) ->
    io:format("starting websocket_owner~n", []),
    receive
	{ok, WebSocket} ->
        self() ! yaws_api:websocket_receive(WebSocket),
	    listen_loop(WebSocket, SyncServer);
	BadValue ->
        {error, BadValue}
    end.

listen_loop(WebSocket, SyncServer) ->
    receive
    {ok, [Message]} -> % used in 'passive mode'
        gen_fsm:send_event(SyncServer, {client_message,
            plMessage:parse_raw_message(Message)}),
        listen_loop(WebSocket, SyncServer);
	{tcp, WebSocket, DataFrame} -> % used in 'active mode'
        % Try to decode all JSON messages 
        % received through the websocket.
        [gen_fsm:send_event(SyncServer, {client_message, 
            plMessage:parse_raw_message(erlang:binary_to_list(Msg))
            }) || Msg <- yaws_websockets:unframe_all(DataFrame, [])],
        listen_loop(WebSocket, SyncServer);
    {send, ToBeSent} ->
        yaws_api:websocket_send(WebSocket, ToBeSent),
        listen_loop(WebSocket, SyncServer);
    {set_active_mode} ->
        yaws_api:websocket_setopts(WebSocket, [{active, true}]),
        listen_loop(WebSocket, SyncServer);
	{tcp_closed, WebSocket} ->
        gen_fsm:send_all_state_event(SyncServer, websocket_closed),
	    io:format("Websocket closed. Terminating listen_loop...~n"),
        {error, websocket_disconnect};
	Any ->
	    io:format("listen_loop received unexpected msg:~p~n", [Any]),
        {error, websocket_disconnect}
    end.

% From YAWS example
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

