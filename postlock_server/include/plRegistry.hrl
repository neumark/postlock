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
    config                  % optional configuration -empty now
}).

%%% ----------------------------------------------------------
%%% postlock_session - the list of postlock session.
%%% ----------------------------------------------------------
-record(postlock_session, {
    id,                     % session id - integer (key)
    session_server,         % PID of state server for session
    creation_date = now()   % format of erlang:now()
}).


%%% ----------------------------------------------------------
%%% ----------------------------------------------------------

-endif. %PL_REGISTRY_HRL
