-module(push_sum_api).

-behaviour(gen_server).

-compile([export_all]).

-record(state, {
    sup, topology_type, nodes = [],
    temp_fail_prob, perm_fail_prob, dead = [],
    round_id, patience, last_msg_sent_at
}).

spawn_summer() ->
    gen_server:cast({global, push_sum_api}, {spawn_summer}).

kickoff() ->
    gen_server:cast({global, push_sum_api}, {kickoff}).

next_round() ->
    Result = gen_server:call({global, push_sum_api}, {next_round}),
    io:format("Result from next-round: ~p~n", [Result]),
    Result.

kill_summer(NodeId) ->
    gen_server:call({global, push_sum_api}, {dead_summer, NodeId}).

send_update_to(NodeId, {S, W}) ->
    gen_server:cast({global, push_sum_api}, {update, NodeId, {S, W}}).

start_link(TopologyType, Patience, TempFailProb, PermFailProb) ->
    gen_server:start_link(
        {global, push_sum_api}, ?MODULE,
        {TopologyType, Patience, TempFailProb, PermFailProb}, []
    ).

init({TopologyType, Patience, TempFailProb, PermFailProb}) ->
    topology:start_link(topology),
    self() ! {start_push_sum_sup},
    {ok, #state{
        topology_type=TopologyType, perm_fail_prob=PermFailProb,
        temp_fail_prob=TempFailProb, round_id=0, patience=Patience,
        last_msg_sent_at=0
    }}.

handle_cast({kickoff}, State = #state{nodes = Nodes}) ->
    NodePid = lists:nth(1, Nodes),
    NodePid ! {kickoff},
    {noreply, State};

handle_cast({update, NodePid, {S, W}}, State = #state{
    nodes = Nodes,
    dead = Dead,
    round_id = RoundId
}) ->
    IsDead = lists:member(NodePid, Dead),
    if IsDead ->
        % io:format("Trying to message dead neighbor~n"),
        {noreply, State};
    true ->
        NodePid ! {update, S, W},
        {noreply, State#state{last_msg_sent_at = RoundId}}
    end;

handle_cast({spawn_summer}, S = #state{
    nodes=Nodes, temp_fail_prob=TempFailProb, perm_fail_prob=PermFailProb
}) ->
    NextId = length(Nodes) + 1,
    {ok, Pid} = supervisor:start_child(
        {global, push_sum_sup}, [NextId, TempFailProb, PermFailProb]
    ),
    {noreply, S#state{nodes = Nodes ++ [Pid]}}.

handle_call({next_round}, _From, S = #state{
    round_id = RoundId, last_msg_sent_at = LastMsgSentAt, patience = Patience,
    nodes = Nodes, dead = Dead
}) ->
    % io:format("Dead = ~p, Nodes = ~p~n", [length(Dead), length(Nodes)]),
    TimeSinceLastMsg = RoundId - LastMsgSentAt,
    % io:format("Last message sent at = ~p~n", [LastMsgSentAt]),
    if
        length(Dead) == length(Nodes) ->
            % All nodes have died (either because they gracefully terminated
            % or abruptly died because of failure).
            % io:format("Round: ~p, Last ")
            % io:format("Converged~n"),
            {reply, {converged, graceful, RoundId}, S#state{round_id = RoundId + 1}};
        
        TimeSinceLastMsg > Patience ->
            {reply, {converged, violent, RoundId}, S#state{round_id = RoundId + 1}};

        true ->
            % io:format("Starting Round #~p~n", [RoundId]),
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

handle_call({dead_summer, NodeId}, _From, S = #state{nodes = Nodes, sup = Sup, dead = Dead}) ->
    % io:format("Killing summer"),
    supervisor:terminate_child(Sup, lists:nth(NodeId, Nodes)),
    NodePid = lists:nth(NodeId, Nodes),
    {reply, ok, S#state{dead = [NodePid] ++ Dead}};

handle_call({get_neighbor, NodeId},
            _From,
            S = #state{topology_type=TopologyType, nodes=Nodes}) ->
    Neighbor = topology:get_neighbor(topology, TopologyType, NodeId, Nodes),
    {reply, Neighbor, S};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_info({start_push_sum_sup}, S) ->
    {ok, Pid} = push_sum_sup:start_link({global, push_sum_sup}, {summer, start_link, []}),
    link(Pid),
    {noreply, S#state{sup = Pid}};
handle_info(_Info, State) ->
    {noreply, State}.
