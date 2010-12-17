{application, telepathy_test,
 [{description,"Test application for telepathy.js server"},
  {vsn,"0.0001"},
  {modules,[
	telepathy_test_app,
	telepathy_test_sup,
	telepathy_test_cb
	]},
  {registered, [telepathy_test_cb]},
  {mod,{telepathy_test_app,[]}},
  {env, []},
  {applications,[kernel, stdlib, yaws, yapp, telepathy_erl]}]}.
