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
-export([init/1, 
        % state functions
        client_init/2, client_init/3, 
        connected/2, connected/3, 
        handle_event/3, handle_sync_event/4, handle_info/3, terminate/3,
        code_change/4]).
-include("plState.hrl").
-record(state, {
          % The id of the client.
          client_id,
          % The PID of the process which owns the websocket, used to
          % push data to the client.
          websocket_owner,
          % PID of the state server, which we forward transactions to
          % and get updates from
          state_server,
          % substate
          substate
}).

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
init([WS_owner, State_server, Client_id]) ->
    % Trigger initial replication of state data to client.
    gen_fsm:send_event(self(), trigger),
    {ok, client_init, #state{
           client_id = Client_id,
           websocket_owner = WS_owner,
           state_server = State_server,
           substate = initial_copy
          }}.

%%--------------------------------------------------------------------
%% === STATE: client_init ===
%% This is the initial state of the server. We remain in this
%% state until the client's state is "up to speed". Then, we advance
%% to 'connected'.
%%--------------------------------------------------------------------

% Copies server state to client.
client_init(trigger, State) when State#state.substate == initial_copy ->
    % Find the number of objects we are going to copy
    % and send this + the client ID to the client.
    NumPublicObjects = gen_server:call(State#state.state_server, {get_num_public_objects}),
    Msg = "{\"client_id\":" ++ erlang:integer_to_list(State#state.client_id) ++ ", " ++
          "\"num_public_objects\":" ++ erlang:integer_to_list(NumPublicObjects) ++ "}",
    State#state.websocket_owner ! {send, Msg},
    NewState = do_initial_copy(State),
    gen_fsm:send_event(self(), copy_finished),
    {next_state, client_init, NewState#state{substate=forward_pending_transformations}};

% Forward the pending server transactions which have accumulated while
% do_initial_copy was running and which have not yet been applied to the
% objects replicated to the client.
client_init({message, server, Msg}, State) when State#state.substate == forward_pending_transformations ->
    % Find the number of objects we are going to copy
    % and send this + the client ID to the client.
    process_pending_transformation(Msg,State),
    {next_state, client_init, State};

% No more waiting transformations, move to 'connected' state.
client_init(copy_finished, State) when State#state.substate == forward_pending_transformations ->
    {next_state, connected, State#state{substate=none}};

% If another message comes in, issue a warning: not perpared for anything else!
client_init(UnhandledMsg, State) when State#state.substate == forward_pending_transformations ->
    error_logger:warning_report("Got plSync:client_init got bad message in forward_pending_transformations", UnhandledMsg).

client_init(_Event, _From, State) ->
    % NOT USED
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% === STATE: connected ====
%% This is the state of the server once initial transfer of server
%% state has been completed. Only the async version (connected/2) is
%% used. 
%% Event should be {client_message|state_server_message, Message}.
%%--------------------------------------------------------------------

connected({client_message, {ok, Msg}}, State) ->
    % Message is the json-decoded term received from the client
    io:format("connected/2 got client message ~p~n",[json_to_transaction(Msg)]),
    {next_state, connected, State};
connected({client_message, Msg}, State) ->
    % Message is the json-decoded term received from the client
    io:format("connected/2 got unparseable client message ~p~n",[Msg]),
    {next_state, connected, State};

connected({state_server_message, Msg}, State) ->
    io:format("connected/2 got server message ~p~n",[Msg]),
    {next_state, connected, State}.


% NOT USED
connected(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.


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
%%% Internal functions
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
%%--------------------------------------------------------------------
do_initial_copy(State) ->
    % STUB -- do nothing for now.
    State.

%%--------------------------------------------------------------------
%% Function: 
%% process_pending_transformation(Transformation, State)-> NewState
%%                                     
%% Description: This function is called by init_client to foward a
%% server transformation received during do_initial_copy. 
%% Only transformations not yet applied to the copied object are
%% forwarded.
%%--------------------------------------------------------------------
process_pending_transformation(_Transformation, State) ->
    % STUB -- do nothing for now.
    State.
   
%%--------------------------------------------------------------------
%%% Input message parsing functions
%%--------------------------------------------------------------------
json_to_transaction(JsonMessage) ->
    try
        {ok, Id} = json_get_value([header, id], JsonMessage),
        {_, AckList} = json_get_value([response, ack], JsonMessage, []),
        {ok, {array, Tlist}} = json_get_value([transformations], JsonMessage),
        {ok, #postlock_transaction{
            id=Id,
            ack = AckList,
            transformations = lists:map(fun interpret_transformation/1, Tlist)
        }}
    catch
        _:_ -> {error, ["failed to parse transaction", JsonMessage]}
    end.
json_get_value(KeyList, JsonMessage) ->
    try
        {ok, json_get_value_1(KeyList, JsonMessage)}
    catch
        _:_ -> {error, ["Error getting key list from JSON message", KeyList, JsonMessage]}
    end.
json_get_value(KeyList, JsonMessage, DefaultValue) ->
    try
        {ok, json_get_value_1(KeyList, JsonMessage)}
    catch
        _:_ -> {default, DefaultValue}
    end.
json_get_value_1([], Value) -> Value;
json_get_value_1([Key|KeyList], {struct,Entries}) ->
    {Key, Value} = lists:keyfind(Key, 1, Entries),
    json_get_value(KeyList, Value).
interpret_transformation(TMsg) ->
    {ok, Cmd} = json_get_value([command], TMsg),
    {_, Oid} = json_get_value([parameters, oid], TMsg, none),
    {_, {struct, ParamList}} = json_get_value([parameters], TMsg, {struct, []}),
    #postlock_transformation{
        cmd = Cmd,
        oid = Oid,
        parameters = ParamList
    }.

