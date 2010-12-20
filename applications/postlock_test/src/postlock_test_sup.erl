%%%-------------------------------------------------------------------
%%% File    : postlock_test_sup.erl
%%% Author  : Peter Neumark
%%% Description : 
%%%
%%%-------------------------------------------------------------------
-module(postlock_test_sup).

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
    Cb = {postlock_test_cb,{postlock_test_cb, start_link,[]},
                         permanent,2000,worker,[postlock_test_cb]},
    {ok,{{one_for_one,0,5}, [Cb]}}.

%%====================================================================
%% Internal functions
%%====================================================================
