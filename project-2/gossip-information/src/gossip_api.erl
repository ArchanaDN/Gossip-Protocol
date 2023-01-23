-module(gossip_api).

-behaviour(gen_server).

-compile([export_all]).

-record(state, {
    sup, topology_type, patience, last_msg_sent_at, dead,
    round_id, rumor, nodes = [], perm_fail_prob, temp_fail_prob
}).


spawn_gossiper() ->
    gen_server:cast({global, gossip_api}, {spawn_gossiper}).

spread_rumor_to(NodePid) ->
    gen_server:cast({global, gossip_api}, {spread_rumor_to, NodePid}).

kill_gossiper(NodeId) ->
    gen_server:call({global, gossip_api}, {dead_gossiper, NodeId}).
    
next_round() ->
    gen_server:call({global, gossip_api}, {next_round}).

start_link(TopologyType, Patience, TempFailProb, PermFailProb, Msg) ->
    gen_server:start_link(
        {global, gossip_api}, ?MODULE,
        {TopologyType, Patience, TempFailProb, PermFailProb, Msg}, []
    ).



init({TopologyType, Patience, TempFailProb, PermFailProb, Msg}) ->
    topology:start_link(topology),
    self() ! {start_gossip_sup},
    {ok, #state{
        round_id=0, patience=Patience, last_msg_sent_at=0,
        dead=[], rumor=Msg, topology_type=TopologyType,
        temp_fail_prob=TempFailProb, perm_fail_prob=PermFailProb
    }}.


handle_cast({spawn_gossiper}, S = #state{nodes = Nodes, temp_fail_prob = TempFailProb, perm_fail_prob = PermFailProb}) ->
    NextId = length(Nodes) + 1,
    {ok, Pid} = supervisor:start_child({global, gossip_sup}, [NextId, TempFailProb, PermFailProb]),
    NewRef = erlang:monitor(process, Pid),
    {noreply, S#state{nodes = Nodes ++ [Pid]}};

handle_cast({spread_rumor_to, NodePid}, S = #state{rumor = Msg, round_id = RoundId, nodes = Nodes, dead = Dead}) ->
    IsDead = lists:member(NodePid, Dead),
    if IsDead ->
        io:format("Trying to message dead neighbor~n"),
        {noreply, S};
    true ->
        NodePid ! {rumor, Msg},
        {noreply, S#state{last_msg_sent_at = RoundId}}
    end.

handle_call({get_neighbor, NodeId},
            _From,
            S = #state{topology_type=TopologyType, nodes=Nodes}) ->
    Neighbor = topology:get_neighbor(topology, TopologyType, NodeId, Nodes),
    {reply, Neighbor, S};

handle_call({dead_gossiper, NodeId}, _From, S = #state{nodes = Nodes, sup = Sup, dead = Dead}) ->
    supervisor:terminate_child(Sup, lists:nth(NodeId, Nodes)),
    NodePid = lists:nth(NodeId, Nodes),
    {reply, ok, S#state{dead = [NodePid] ++ Dead}};

handle_call({next_round}, _From, S = #state{round_id = RoundId, last_msg_sent_at = LastMsgSentAt, patience = Patience, nodes = Nodes, dead = Dead}) ->
    io:format("Dead = ~p, Nodes = ~p~n", [length(Dead), length(Nodes)]),
    TimeSinceLastMsg = RoundId - LastMsgSentAt,
    io:format("Last message sent at = ~p~n", [LastMsgSentAt]),
    if
        length(Dead) == length(Nodes) ->
            % All nodes have died (either because they gracefully terminated
            % or abruptly died because of failure).
            {reply, {converged, graceful, RoundId}, S#state{round_id = RoundId + 1}};

        TimeSinceLastMsg > Patience ->
            {reply, {converged, violent, RoundId}, S#state{round_id = RoundId + 1}};

        true ->
            io:format("Starting Round #~p~n", [RoundId]),
            MsgsSent = lists:filter(
                fun(NodePid) ->
                    IsDead = lists:member(NodePid, Dead),
                    if
                        IsDead ->
                            false;
                        true ->
                            NodePid ! {next_round, NodePid},
                            true
                    end
                end,
                Nodes
            ),
            {reply, {not_converged, invalid, RoundId}, S#state{round_id = RoundId + 1}}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.



handle_info({start_gossip_sup}, S) ->
    {ok, Pid} = gossip_sup:start_link({global, gossip_sup}, {gossiper, start_link, []}),
    link(Pid),
    {noreply, S#state{sup = Pid}};
handle_info(_Info, State) ->
    {noreply, State}.
