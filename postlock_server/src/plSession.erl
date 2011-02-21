%%%-------------------------------------------------------------------
%%% File    : plSession.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description : This module implements the session server. It is
%%% a gen_server with the following responsibilities:
%%% - maintains the list of participants
%%% - maps participant ids to erlang pids
%%% - delivers messages to a participant
%%% - spanws new plGateway process for connecting client
%%%
%%% Created : 15 Feb 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plSession).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SYSTEM_USER, "postlock_system").
-define(SESSION_SERVER_PARTICIPANT_ID, 0).

-include("plSession.hrl").
% needed for #pl_client_msg
-include("plMessage.hrl").

-record(state, {
    % session id
    session_id,
    % maps participants to plGateway PIDs
    participants=gb_trees:empty(),
    % callback process, used for eg: authentication
    callback
}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ServerData = [_SessionId, _CallbackProcess]) ->
    gen_server:start_link(?MODULE, ServerData, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([SessionId, CallbackProcess]) ->
    % Don't die on 'EXIT' signal
    process_flag(trap_exit, true),
    State0 = #state{
        session_id=SessionId,
        callback=CallbackProcess},
    case add_system_participants(State0) of
    {ok, NewState} ->
        {ok, NewState};
    {error, Reason} ->
        {stop, {error_starting_system_participants, Reason}}
    end.


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% Handle incoming connection requests from new pariticpants.
handle_call({connect_client, Connection}, _From, State) ->
    ParticipantId = gb_trees:size(State#state.participants),
    {Reply, NewState} = case plGateway:start_link([
        self(), ParticipantId, Connection]) of
        {ok, Gateway} -> 
            NS = add_participant(State, #pl_participant{
                % No username yet, that comes after authentication.
                id = ParticipantId,
                process_id = Gateway
            }),
            WSOwner = gen_fsm:sync_send_event(Gateway, {get_websocket_owner}),
            {{ok, {websocket, WSOwner, passive}},
              NS};
        Err = {error, _Reason} ->
            {Err, State}
    end,
    {reply, Reply, NewState};
%% Proxies requests to the callback server. 
handle_call({get_callback, _CbName} = Req, _From, State) ->
   Reply = gen_server:call(State#state.callback, Req),
   {reply, Reply, State};

%% Returns session id to caller.
handle_call({get_session_id}, _From, #state{session_id=Sid}=State) ->
   {reply, Sid, State};

handle_call(Request, From, State) ->
    error_logger:warning_report(["plSession:handle_call/3: unhandled message",
        {request, Request},
        {from, From},
        {state, State}]),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%% Update participant ID.
handle_cast({update_participant_data, #pl_participant{id=Id} = ParticipantData}, 
    #state{participants = P} = State) ->
    % Only username update is currently supported
    % TODO: check for case when Id doesnt refer to valid participant.
    NewState = State#state{participants = 
                    gb_trees:update(Id,ParticipantData,P)}, 
    {noreply, NewState};

%% Called when a client disconnects.
handle_cast(
    {disconnect, 
        {#pl_participant{id=Id} = ParticipantData,
         Reason,
         Details}}, 
    #state{participants = P} = State) ->
    % TODO: check that participant is in list of participants
    error_logger:info_report(["disconnecting participant from session", 
        ParticipantData, Reason, Details]),
    NewState = State#state{participants = 
                    gb_trees:delete(Id,P)}, 
    {noreply, NewState};

%% Delivers messages between participants
handle_cast({deliver_message, #pl_client_msg{to=To}=Msg}, 
    #state{participants=Participants}=State) ->
    case To of 
        ?SESSION_SERVER_PARTICIPANT_ID ->
            io:format("Session server got message ~p~n", [Msg]);
        OtherParticipant ->
            % TODO: send error if deliver_message returns {error, _}
            deliver_message(Msg, Participants)
    end,
    {noreply, State};
 
handle_cast(Msg, State) ->
    error_logger:warning_report(["plSession:handle_cast/2: unhandled message",
        {message, Msg},
        {state, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', _Pid, {#pl_participant{id=ParticipantId}, Reason}}, 
    #state{participants=Participants}=State) ->
    io:format("Participant #~p disconnected. Reason: ~p~n",[ParticipantId, Reason]),
    {noreply, State#state{participants=gb_trees:delete(ParticipantId, Participants)}};

handle_info({'EXIT', Pid, Reason}, State) ->
    % TODO: handle state server crashes.
    io:format("Linked process with PID ~p died! Reason: ~p~n",[Pid, Reason]),
    {noreply, State};

handle_info(Info, State) ->
    error_logger:warning_report(["plSession:handle_info/2: unhandled message",
        {message, Info},
        {state, State}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%  Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add_system_participants(#state) -> {ok, Participants} |
%%                         {error, Reason} |
%% Description: spawns the postlock 'system participants', which are
%% the mandatory participants for any functioning session.
%% These are:
%%   Session Server (this process)
%%   State Server (lives in plState, spawned here)
%%--------------------------------------------------------------------
add_system_participants(#state{session_id=SessionId}=State) ->
    %% TODO: start a configurable set of servers
    %% instead of a hardcoded list.
    %% attempt to spawn state server
    case plState:start_link([SessionId]) of
        {ok, Pid} -> {ok, 
            lists:foldl( 
                fun({ParticipantId, ParticipantPid}, S) ->
                    add_participant(S, #pl_participant{
                        id=ParticipantId,
                        username=?SYSTEM_USER,
                        process_id=ParticipantPid
                    })
                end,
                State,
                [{?SESSION_SERVER_PARTICIPANT_ID, self()}, {1, Pid}])};
            Err = {error, _Reason} -> Err
    end.    

add_participant(#state{participants=Participants}=State, 
    #pl_participant{id=ParticipantId}=NewParticipant) ->
    State#state{participants=gb_trees:insert(
        ParticipantId,
        NewParticipant,
        Participants)}.

deliver_message(#pl_client_msg{to=To}=Msg, Participants) ->
    case gb_trees:is_defined(To, Participants) of
       true ->
           #pl_participant{process_id = Pid} = gb_trees:get(To, Participants),
           Pid ! {participant_message, Msg};
       false ->
           {error, {no_such_participant, To}}
    end.
