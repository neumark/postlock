%%%-------------------------------------------------------------------
%%% File    : plRegistry.erl
%%% Author  : Peter Neumark 
%%% Description : 
%%%
%%% Created :  9 Dec 2010 by Peter Neumark 
%%%-------------------------------------------------------------------
-module(plRegistry).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, drop_tables/0]).
-include("plRegistry.hrl").
-define(SERVER, ?MODULE).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init(_) ->
    % TODO: read state from mnesia
    process_flag(trap_exit, true),
    make_tables(),
    init_postlock_global(),
    {ok, []}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% Creates a new sync server with a brand new client id, then registers
%% the sync server with the state server responsible for the session.
handle_call({new_session, Callback}, _From, State) ->
    Reply = create_session(Callback),
    {reply, Reply, State};

handle_call(Request, _From, State) ->
    io:format("Unhandled call sent to plRegistry:handle_call - ~p~n",[Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("Unhandled call sent to plRegistry:handle_cast - ~p~n",[Msg]),
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
    %% Todo: handle EXIT messages from sync and state servers!
    io:format("Unhandled request sent to plRegistry:handle_info - ~p~nin state~p~n",[Info, State]),
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_next_session_id() ->
    F = fun() ->
        [OldGlobalRec] = mnesia:read(postlock_global, 0),
        SessionId = OldGlobalRec#postlock_global.next_session_id,
        NewGlobalRec = OldGlobalRec#postlock_global{next_session_id=SessionId+1},
        mnesia:write(NewGlobalRec),
        SessionId
    end,
    {atomic, S} = mnesia:transaction(F),
    S.

%%--------------------------------------------------------------------
%% Function: create_session(Callback) -> 
%%                                      {ok, StateServer, SessionId} |
%%                                      {error, Reason}
%% Description: 
%% Creates a new state server and corresponding session. The specified
%% callback server is connected. A new record is created in the 
%% postlock_session mnesia table.
%%--------------------------------------------------------------------
create_session(Callback) ->
    SessionId = get_next_session_id(),
    case plSession:start_link([SessionId, Callback]) of
        {ok, Pid} = Ret ->
            %% write new session record to mnesia
            mnesia:transaction(fun() -> mnesia:write(#postlock_session{
                id = SessionId,
                session_server = Pid
            }) end),
            Ret;
        {error, _Reason} = Err -> Err
    end.

make_tables() ->
    DefaultArgs = ?POSTLOCK_DEFAULT_TABLE_ARGS,
    [make_table_1(TableName, 
        [{attributes, Fields} | Args ++ DefaultArgs]) 
        || {TableName, Fields, Args} <- [
        % list of table names, with list of arguments for table
        {postlock_global, record_info(fields,postlock_global), []},
        {postlock_session, record_info(fields,postlock_session), []}
    ]],
    ok.

make_table_1(TableName, Args) ->
    case ets:info(TableName) of
        undefined ->
            error_logger:info_report("Creating table "++ erlang:atom_to_list(TableName) ++ "~n"),
	    mnesia:create_table(TableName, Args);
        _ ->
            ok
    end.

init_postlock_global() ->
    %% insert the single record into postlock_global
    %% if none exist yet
    mnesia:transaction(fun() -> 
        case mnesia:last(postlock_global) of
            0 -> ok;
            _ -> mnesia:write(#postlock_global{})
        end
    end).

drop_tables() ->
    % drop tables for each session:
    {atomic, SessionId} = mnesia:transaction(fun() ->
        [OldGlobalRec] = mnesia:read(postlock_global, 0),
        SessionId = OldGlobalRec#postlock_global.next_session_id,
        SessionId
    end),
    [plState:drop_tables(Sid) || Sid <- lists:seq(0, SessionId -1)],
	mnesia:delete_table(postlock_global),
	mnesia:delete_table(postlock_session).
