%%%-------------------------------------------------------------------
%%% File    : plCallbackMgr.erl
%%% Author  : Peter Neumark <neumark@postlock.org>
%%% Description : 
%%%
%%% Created : 17 Feb 2011 by Peter Neumark <neumark@postlock.org>
%%%-------------------------------------------------------------------
-module(plCallbackMgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    callbacks = gb_trees:empty()
}).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

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
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_callback, Name}, _From, #state{callbacks=Cb}=State) ->
    Reply = case gb_trees:is_defined(Name, Cb) of
        true ->
            {ok, gb_trees:get(Name, Cb)};
        false ->
            {error, no_defined_callback}
    end,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    io:format("~p:handle_call called with:~nRequest: ~p~nFrom:~p~nState:~p~n", 
        [?MODULE, Request, From, State]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({set_callback, {Name, Fun}}, State) ->
    NewState = case gb_trees:is_defined(Name, State#state.callbacks) of
        true ->
            State#state{callbacks = gb_trees:update(
                Name, Fun, State#state.callbacks)};
        false ->
            State#state{callbacks = gb_trees:insert(
                Name, Fun, State#state.callbacks)}
    end,
    {noreply, NewState};

handle_cast(Msg, State) ->
    io:format("~p:handle_cast called with:~nMsg:~p~nState:~p~n", 
        [?MODULE, Msg, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    io:format("~p:handle_info called with:~nInfo:~p~nState:~p~n", 
        [?MODULE, Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    io:format("~p:terminate called with:~nReason:~p~nState:~p~n", 
        [?MODULE, Reason, State]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    io:format("~p:code_change called with:~nOldVsn:~p~nState:~p~nExtra:~p~n", 
        [?MODULE, OldVsn, State, Extra]),
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%
