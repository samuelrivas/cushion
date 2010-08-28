{application, cushion,
 [{description, "CouchDB adaptation layer"},
  {vsn, "devel"},
  {modules, [cushion_couch_api, cushion_util]},
  {registered, []},
  {applications, [kernel, stdlib, lhttpc]},
  {env, []}]}.
