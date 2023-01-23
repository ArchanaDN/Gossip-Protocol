-module(gossiper).
-behaviour(gen_server). 

-include("../include/hyperparams.hrl").
-compile([export_all]).

-record(state, {id, rumor, rumor_repeats, connection_alive, temp_fail_prob, perm_fail_prob}).

start(N) ->
    gossip_sup:start_child(N).

revive(Pid) ->
    gen_server:call(Pid, {revive}).

stop(Name) ->
    gen_server:call({local, Name}, stop).

start_link(Id, TempFailProb, PermFailProb) ->
    gen_server:start_link(
        {local, list_to_atom(integer_to_list(Id))}, ?MODULE,
        {Id, TempFailProb, PermFailProb}, []
    ).

init({Id, TempFailProb, PermFailProb}) ->
    {ok, #state{id=Id, rumor_repeats=0, temp_fail_prob=TempFailProb, perm_fail_prob=PermFailProb, connection_alive=true}}.

handle_call({revive}, _From, S = #state{id = NodeId}) ->
    io:format("Node ~p revived ~n", [NodeId]),
    {reply, ok, S#state{connection_alive = true}};

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({rumor, Msg}, S=#state{id = NodeId, connection_alive = ConnectionAlive, rumor_repeats = RumorRepeats}) ->
    if ConnectionAlive ->
        io:format("Node ~p received rumor for the ~pth time: ~p~n", [NodeId, RumorRepeats + 1, Msg]),
        {noreply, S#state{rumor = Msg, rumor_repeats = RumorRepeats + 1}};
    true ->
        {noreply, S}
    end;

handle_info({next_round, MyPid},S=#state{
    id = NodeId, connection_alive = ConnectionAlive,rumor = Msg,
    rumor_repeats = RumorRepeats, perm_fail_prob = PermFailProb,
    temp_fail_prob = TempFailProb
}) ->
    if
        ConnectionAlive ->
            % With a small probability, die.
            RandPerm = rand:uniform(),
            if
                RandPerm < PermFailProb -> gossip_api:kill_gossiper(NodeId);
                true -> nothing
            end,

            if RumorRepeats < 10 ->
                Neighbor = gen_server:call({global, gossip_api}, {get_neighbor, NodeId}),
                gossip_api:spread_rumor_to(Neighbor);
            true ->
                io:format("Node ~p will not be passing it on.~n", [NodeId]),
                gossip_api:kill_gossiper(NodeId)
            end,

            % With a small probability, temporarily disconnect.
            RandTemp = rand:uniform(),
            if
                RandTemp < TempFailProb ->
                    io:format("Node ~p temporarily dying~n", [NodeId]),
                    timer:apply_after(1, ?MODULE, revive, [MyPid]),
                    {noreply, S#state{connection_alive=false}};
                true ->
                    {noreply, S}
            end;
        
    true ->
            % Simulate being temporarily disconnected.
            {noreply, S}

    end.
