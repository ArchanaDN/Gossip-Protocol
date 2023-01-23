-module(gossip_sup).
-behaviour(supervisor).
-compile([export_all]).

start_link(Name, MFA = {_,_,_}) ->
    supervisor:start_link(Name, ?MODULE, MFA).


init({M,F,A}) ->
    MaxRestart = 5,
    MaxTime = 3600,
    {ok, {{simple_one_for_one, MaxRestart, MaxTime},
          [{gossiper,
            {M,F,A},
            temporary, 5000, worker, [M]}]}}.
