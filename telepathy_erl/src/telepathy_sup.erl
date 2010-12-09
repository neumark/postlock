%%%-------------------------------------------------------------------
%%% File    : telepathy_sup.erl
%%% Author  : Peter Neumark
%%% Description : 
%%%
%%%-------------------------------------------------------------------
-module(telepathy_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_) ->
    io:format("telepathy_start:start_link called~n",[]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    StateServerFactory = {telepathy_state_server_factory,{telepathy_state_server_factory, start_link,[]},
                         permanent,2000,worker,[telepathy_state_server_factory]},
    SyncServerFactory = {telepathy_sync_server_factory,{telepathy_sync_server_factory, start_link, []},
                   	 permanent,2000,worker,[telepathy_sync_server_factory]},
    {ok,{{one_for_one,0,5}, [StateServerFactory, SyncServerFactory]}}.

%%====================================================================
%% Internal functions
%%====================================================================
