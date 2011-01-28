%%% ----------------------------------------------------------
%%% File:   plError.hrl
%%% Author: Peter Neumark
%%% Description: Error code macros.
%%% Created :  20 Jan 2011 by Peter Neumark
%%% ----------------------------------------------------------
-hrl_author('neumark').

-ifndef(PL_ERROR_HRL).
-define(PL_ERROR_HRL,1).

-define(BAD_SYNC_STATE_FOR_CLIENT_MESSAGE, 
    {100, "Unexpcted message: Not yet in connected state!"}).
-define(ERROR_PARSING_TRANSACTION,
    {102, "Error parsing transaction"}).

-endif. %PL_ERROR_HRL

