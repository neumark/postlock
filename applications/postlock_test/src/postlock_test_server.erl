%%%-------------------------------------------------------------------
%%% File    : postlock_test_server.erl
%%% Author  : Peter Neumark 
%%% Description : 
%%%
%%% Created :  9 Dec 2010 by Peter Neumark 
%%%-------------------------------------------------------------------
-module(postlock_test_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    session_server,
    sessionid,
    callback_server
}).
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
init([]) ->

    % start a new postlock session for test clients.
    {ok, CbServer} = plCallbackMgr:start_link(),
    % initialize authentication functions in CbServer
    gen_server:cast(CbServer,{set_callback, {auth_challenge, fun trivial_auth_challenge/0}}),
    gen_server:cast(CbServer,{set_callback, {authenticate, fun trivial_authenticate/2}}),
    case gen_server:call(plRegistry,{new_session, CbServer}) of
        {ok, {SessionId, SessionServer}} -> 
            {ok, #state{
                sessionid=SessionId,
                session_server=SessionServer,
                callback_server=CbServer
                }};
        {error, Reason} -> {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_session}, _From, State) ->
    {reply, {State#state.sessionid, State#state.session_server}, State};

handle_call(Request, _From, State) ->
    io:format("Unhandled request sent to postlock_test_cb:handle_call - ~p~n",[Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    io:format("Unhandled cast sent to postlock_test_cb:handle_cast/2 - ~p~n",[Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("Unhandled info sent to postlock_test_cb:handle_info/2 - ~p~n",[Info]),
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

trivial_auth_challenge() ->
    {struct, [
        {"challenge_type", "trivial"}
    ]}.

trivial_authenticate(_Challenge, Response) ->
    Username = json:obj_fetch(username, Response),
    Password = json:obj_fetch(password, Response),
    case Username == Password of
        true -> {ok, Username};
        false -> {error, bad_password}
    end.
