{application, node_manager,
 [{description, "FORSE Node Manager"},
  {vsn, "1.0"},
  {modules, [node_mgr_app, node_mgr_sup, node_manager]},
  {registered, [node_manager]},
  {applications, [kernel, stdlib]},
  {mod, {node_mgr_app, []}}
 ]}.
