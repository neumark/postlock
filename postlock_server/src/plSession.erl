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

-define(SYSTEM_USER, postlock_system).
-include("plSession.hrl").

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
handle_call({connect_client, Connection}, _From, State) ->
    ParticipantId = gb_trees:size(State#state.participants),
    {Reply, NewState} = case plGateway:start_link([
        self(), ParticipantId, Connection]) of
        {ok, Pid} -> 
            NS = add_participant(State, #pl_participant{
                % No username yet, that comes after authentication.
                id = ParticipantId,
                process_id = Pid
            }),
            {ok,NS};
        Err = {error, _Reason} ->
            {Err, State}
    end,
    {reply, Reply, NewState};

handle_call({get_callback, _CbName} = Req, _From, State) ->
   Reply = gen_server:call(State#state.callback, Req),
   {reply, Reply, State};
 
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
handle_cast({update_participant_data, #pl_participant{id=Id} = ParticipantData}, 
    #state{participants = P} = State) ->
    % Only username update is currently supported
    % Ignores bad calls
    % TODO: check for case when Id doesnt refer to valid participant.
    NewState = State#state{participants = 
                    gb_trees:update(Id,ParticipantData,P)}, 
    {noreply, NewState};

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
handle_info({'EXIT', Pid, Reason}, State) ->
    %% Todo: handle EXIT messages from sync and state servers!
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
                [{0, self()}, {1, Pid}])};
            Err = {error, _Reason} -> Err
    end.    

add_participant(#state{participants=Participants}=State, 
    #pl_participant{id=ParticipantId}=NewParticipant) ->
    State#state{participants=gb_trees:insert(
        ParticipantId,
        NewParticipant,
        Participants)}.

