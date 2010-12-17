{application, postlock_server,
 [{description,"Postlock server"},
  {vsn,"0.0001"},
  {modules,[
    plSync, 
    plRegistry,
    plState, 
    plApp,
    plApi]},
  {registered, [plRegistry]},
  {mod,{plApp,[]}},
  {env, []},
  {applications,[kernel, stdlib, yaws, yapp]}]}.
