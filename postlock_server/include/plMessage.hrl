%%% ----------------------------------------------------------
%%% File:   plMessage.hrl
%%% Author: Peter Neumark
%%% Description: Contains records and constants used by
%%% plMessage module.
%%%
%%% ----------------------------------------------------------
-hrl_author('neumark').

-ifndef(PL_MESSAGE_HRL).
-define(PL_MESSAGE_HRL,1).

%%% ----------------------------------------------------------
%%% postlock_message - the contents of a parsed json message
%%% ----------------------------------------------------------
-record(postlock_message, {
    type,                   % atom, eg. transaction
    header = [], 
    body = []               
}).

-endif. %PL_MESSAGE_HRL

