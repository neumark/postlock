%%%-------------------------------------------------------------------
%%% File    : sync_server.erl
%%% Author  : Peter Neumark
%%% Description : 
%%% A sync_server process handles the dialog with the each client.
%%% YAWS gives us the JSON-encoded message from the client, then
%%% it's up to sync_server to:
%%% - Handle initialization of newly connected clients
%%% - Read ACKs for server transactions from the message, and
%%%   update the server's understanding of the client's state.
%%% - Forward client transactions to the state server.
%%% - Push modifications coming from state-server to the client
%%%   through websockets.
%%%
%%% Created :  7 Dec 2010 by Peter Neumark
%%%-------------------------------------------------------------------
-module(telepathy_sync_server).
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

-record(state, {
          % The id of the client.
          client_id,
          % The PID of the process which owns the websocket, used to
          % push data to the client.
          websocket_owner,
          % PID of the state server, which we forward transactions to
          % and get updates from
          state_server,
          % The list of server transactions sent to the client which
          % have not yet been acknowledged.
          pending_transactions,
          % Contains arbitrary data, used by the current FSM state.
          current_state_data
          
}).

-define(SERVER, telepathy_sync_server).
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
    {ok, client_init, #state{
           client_id = Client_id,
           websocket_owner = WS_owner,
           state_server = State_server
          }}.

%%--------------------------------------------------------------------
%% === STATE: client_init ===
%% This is the initial state of the server. We remain in this
%% state until the client's state is "up to speed". Then, we advance
%% to 'connected'.
%% 
%%--------------------------------------------------------------------
client_init(_Event, State) ->
    io:format("STUB client_init complete~n",[]),
    {next_state, connected, State}.

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

connected({client_message, Msg}, State) ->
    % Message is the json-decoded term received from the client
    io:format("connected/2 got client message ~p~n",[Msg]),
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

