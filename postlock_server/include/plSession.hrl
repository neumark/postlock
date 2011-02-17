%%% ----------------------------------------------------------
%%% File:   plSession.hrl
%%% Author: Peter Neumark
%%% Description: Contains records and constants used by
%%% plSession module.
%%%
%%% ----------------------------------------------------------
-hrl_author('neumark').

-ifndef(PL_SESSION_HRL).
-define(PL_SESSION_HRL,1).
-record(pl_participant, {
    id,                % participant id
    username = unknown,% defined after authentication
    process_id,        % erlang PID
    joined = now()
}).


%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
-endif. %PL_SESSION_HRL

