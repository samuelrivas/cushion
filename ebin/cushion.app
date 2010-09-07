{application, cushion,
 [{description, "CouchDB adaptation layer"},
  {vsn, "devel"},
  {modules, [cushion, cushion_couch_api, cushion_util, cushion_json]},
  {registered, []},
  {applications, [kernel, stdlib, lhttpc, ktuo]},
  {env, []}]}.
