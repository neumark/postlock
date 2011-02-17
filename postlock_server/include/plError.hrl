%%% ----------------------------------------------------------
%%% File:   plError.hrl
%%% Author: Peter Neumark
%%% Description: Error code macros.
%%% Created :  20 Jan 2011 by Peter Neumark
%%% ----------------------------------------------------------
-hrl_author('neumark').

-ifndef(PL_ERROR_HRL).
-define(PL_ERROR_HRL,1).
-define(ERROR2JSON(Err),
    {struct, [
        {"code", element(1,Err)},
        {"message", element(2, Err)}
    ]}
).
%%% ----------------------------------------------------------
%%% 1XX ERRORS: issued by plGateway
%%% ----------------------------------------------------------
-define(PL_ERR_BAD_SYNC_STATE_FOR_CLIENT_MESSAGE, 
    {100, "Unexpcted message: Not yet in connected state!"}).
-define(PL_ERR_ERROR_PARSING_TRANSACTION,
    {102, "Error parsing transaction"}).
-define(PL_ERR_AUTH_FAILURE,
    {103, "Authentication failed"}).


-endif. %PL_ERROR_HRL

