-module(summer).

-behaviour(gen_server).

-compile([export_all]).

-record(state, {
    id, s, w, num_iters_unchanged, temp_fail_prob, perm_fail_prob, connection_alive,
    waiting_to_send
}).

revive(Pid) ->
    gen_server:call(Pid, {revive}).

start(N) ->
    push_sum_sup:start_child(N).

stop(Name) ->
    gen_server:call(Name, stop).

start_link(Id, TempFailProb, PermFailProb) ->
    gen_server:start_link(
        {local, list_to_atom(integer_to_list(Id))}, ?MODULE,
        {Id, TempFailProb, PermFailProb}, []
    ).

init({Id, TempFailProb, PermFailProb}) ->
    io:format("Spawning summer: ~p~n", [Id]),
    {ok,
     #state{id = Id,
            s = Id,
            w = 1,
            num_iters_unchanged = 0,
            temp_fail_prob = TempFailProb,
            perm_fail_prob = PermFailProb,
            connection_alive = true,
            waiting_to_send = false}}.

handle_call({revive}, _From, S = #state{id = NodeId}) ->
    io:format("Node ~p revived ~n", [NodeId]),
    {reply, ok, S#state{connection_alive = true}};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({kickoff},
            State = #state{id = NodeId,
            s = OldS,
            w = OldW,
            num_iters_unchanged = OldNumItersUnchanged}) ->
    Neighbor = gen_server:call({global, push_sum_api}, {get_neighbor, NodeId}),
    push_sum_api:send_update_to(Neighbor, {OldS / 2, OldW / 2}),
    {noreply, State#state{waiting_to_send = true}};

handle_info({update, S, W},
            State = #state{id = NodeId,
            s = OldS,
            w = OldW,
            num_iters_unchanged = OldNumItersUnchanged,
            connection_alive = ConnectionAlive,
            perm_fail_prob = PermFailProb,
            temp_fail_prob = TempFailProb}) ->
    NewS = OldS + S,
    NewW = OldW + W,

    % Calculate s/w ratio and also the delta between the old and new ratios.
    OldRatio = OldS / OldW,
    NewRatio = NewS / NewW,

    if abs(NewRatio - OldRatio) < 10.0e-10 ->
        NewNumItersUnchanged = OldNumItersUnchanged + 1,
        WaitingToSend = false;
    true ->
        NewNumItersUnchanged = 0,
        WaitingToSend = true
    end,

    {noreply, State#state{
        s = NewS / 2,
        w = NewW / 2,
        num_iters_unchanged = NewNumItersUnchanged,
        waiting_to_send = WaitingToSend
    }};

handle_info({next_round, MyPid},
            State = #state{id = NodeId,
                       s = S,
                       w = W,
                       num_iters_unchanged = NumItersUnchanged,
                       connection_alive = ConnectionAlive,
                       perm_fail_prob = PermFailProb,
                       temp_fail_prob = TempFailProb,
                       waiting_to_send = WaitingToSend}) ->
    if
        ConnectionAlive and WaitingToSend ->
            % With a small probability, die.
            RandPerm = rand:uniform(),
            if
                RandPerm < PermFailProb -> push_sum_api:kill_summer(NodeId);
                true -> nothing
            end,

            % With a small probability, temporarily disconnect.
            RandTemp = rand:uniform(),
            if
                RandTemp < TempFailProb ->
                    % io:format("Node ~p temporarily dying~n", [NodeId]),
                    timer:apply_after(1, ?MODULE, revive, [MyPid]),
                    {noreply, State#state{connection_alive=false}};
                true ->
                    {noreply, State}
            end,

            % If the ratio hasn't changed in the last three iterations, then stop
            % propagating messages.
            if
                NumItersUnchanged == 3 ->
                    % io:format("Ratio for ~p hasn't changed in 3 iters~n", [NodeId]),
                    push_sum_api:kill_summer(NodeId),
                    {noreply, State};
                true ->
                    Neighbor = gen_server:call({global, push_sum_api}, {get_neighbor, NodeId}),
                    push_sum_api:send_update_to(Neighbor, {S, W}),
                    {noreply,
                    State#state{s = S,
                            w = W,
                            num_iters_unchanged = NumItersUnchanged + 1,
                            waiting_to_send = false}}
            end;
        true ->
            % io:format("Either connection dead or not waiting to send~n"),
            {noreply, State}
    end.
