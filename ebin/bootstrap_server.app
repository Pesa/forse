{application, bootstrap_server,
 [{description, "FORSE Bootstrap Server"},
  {vsn, "1.0"},
  {modules, [bootstrap_srv_app, bootstrap_srv_sup, bootstrap_server]},
  {registered, [bootstrap_server]},
  {applications, [kernel, stdlib]},
  {mod, {bootstrap_srv_app, []}}
 ]}.
