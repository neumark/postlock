{application, telepathy_erl,
 [{description,"Telepathy.js server"},
  {vsn,"0.0001"},
  {modules,[telepathy_sync_server, telepathy_state_server, telepathy_app]},
  {registered, []},
  {mod,{telepathy_app,[]}},
  {env, []},
  {applications,[kernel, stdlib, yaws, yapp]}]}.
