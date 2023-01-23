{application, main,
 [{description, "Gossip by Amogh Mannekote and Gloria Katuka"},
  {vsn, "1.0.0"},
  {registered, []},
  {mod, {main, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[
    
  ]},
  {modules, [main, gossip_api, gossip_sup, gossiper, topology, push_sum_api, push_sum_sup, summer]},
  {licenses, ["Apache 2.0"]},
  {links, []},
  {mod, [main]}
 ]}.
