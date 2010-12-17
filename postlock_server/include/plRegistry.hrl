%%% ----------------------------------------------------------
%%% File:   plRegistry.hrl
%%% Author: Peter Neumark
%%% Description: Contains records and constants used by
%%% plRegistry module.
%%%
%%% ----------------------------------------------------------
-hrl_author('neumark').

-ifndef(PL_REGISTRY_HRL).
-define(PL_REGISTRY_HRL,1).
-define(POSTLOCK_DEFAULT_TABLE_ARGS, [{disc_copies, [node()]}]).
%%% ----------------------------------------------------------
%%% postlock_global - table to keep track of global 
%%% counters like
%%% current session id or current client id.
%%% ----------------------------------------------------------
-record(postlock_global, {
    key                    = 0, % just to keep mnesia happy
    next_session_id        = 0,
    config                  % optional configuration
}).

%%% ----------------------------------------------------------
%%% postlock_client - table to keep track of clients
%%% each client belongs to a single session. If a single
%%% application participates in several sessions, then it will
%%% need a separate user id for each session.
%%% ----------------------------------------------------------
-record(postlock_client, {
    id,                     % {user_id, session_id} (key)
    created,                % output of erlang:now(), 
                            % date the user joined
    sync_server_pid         % pid of sync server of client

}).

%%% ----------------------------------------------------------
%%% postlock_session - the list of postlock session.
%%% ----------------------------------------------------------
-record(postlock_session, {
    id,                     % integer (key)
    state_server,       % PID of state server for session
    creation_date          = now(),   % format of erlang:now()
    next_client_id         = 0,
    next_transformation_id = 0,
    clients                = gb_trees:empty() % contains 
                            % clientid -> #postlock_client
}).


%%% ----------------------------------------------------------
%%% ----------------------------------------------------------

-endif. %PL_REGISTRY_HRL
