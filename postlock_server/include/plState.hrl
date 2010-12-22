%%% ----------------------------------------------------------
%%% File:   plRegistry.hrl
%%% Author: Peter Neumark
%%% Description: Contains records and constants used by
%%% plState module.
%%%
%%% ----------------------------------------------------------
-hrl_author('neumark').

-ifndef(PL_STATE_HRL).
-define(PL_STATE_HRL,1).
%%% ----------------------------------------------------------
%%% postlock_object - this is where the session data is 
%%% stored.
%%% ----------------------------------------------------------
-record(postlock_object, {
    id,                     % {clientid, objectid} (key)
    type,                   % data | dict | list
    contents,               % opaque, content depends upon
                            % the type of object
    parents = []            % a list of ids who are parents of
                            % this node.
}).

%%% ----------------------------------------------------------
%%% postlock_transformation - the list of all transformations
%%% which have been committed on the shared state
%%% ----------------------------------------------------------
-record(postlock_transformation, {
    id,                     % integer (key)
    user,                   % id of user who submitted t
    cmd,                    % command of transformation
    oid,                    % may be undefined for 'create'
    parameters,             % parameters to the command
    extra_data              % optional extra data
}).

%%% ----------------------------------------------------------
%%% postlock_transaction - contains a group of transformations
%%% received by the sync server from a client.
%%% ----------------------------------------------------------
-record(postlock_transaction, {
    id,                     % integer (key)
    user,                   % id of user who submitted t
    ack = []                % a list of ACKs sent by the client
    transformations = []    % list of transformations
    received = now()        % timestamp of transaction
}).

%%% ----------------------------------------------------------
%%% ----------------------------------------------------------
-endif. %PL_STATE_HRL

