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

-define(RECORD2JSON(Record_name, Instance),
    element(1, lists:foldl(
        fun(FieldName, {Json, FieldNo}) -> 
            Value = element(FieldNo, Instance),
            { case Value of 
                  undefined -> Json;
                  _ -> json:obj_store(FieldName, Value, Json)
              end, FieldNo + 1}
        end,
        {json:obj_new(), 1},
        record_info(fields, Record_name)))).

%%% ----------------------------------------------------------
%%% postlock_message - the contents of a parsed json message
%%% ----------------------------------------------------------
-record(pl_client_msg, {
    from,               % participant id (integer)
    to,                 % participant id (integer)
    type,               % message type (string)
    body                 % message payload
}).

-endif. %PL_MESSAGE_HRL

