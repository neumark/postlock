{application, postlock_test,
 [{description,"Test application for postlock server"},
  {vsn,"0.0001"},
  {modules,[
	postlock_test_app,
	postlock_test_sup,
	postlock_test_server
	]},
  {registered, [postlock_test_cb]},
  {mod,{postlock_test_app,[]}},
  {env, []},
  {applications,[kernel, stdlib, yaws, yapp, postlock_server]}]}.
