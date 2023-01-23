-module(main).
-behavior(application).

-include("../include/hyperparams.hrl").

-compile([export_all]).


start(normal, _Args) ->
    Problem = application:get_env(main, problem, ?PROBLEM),
    Topology = application:get_env(main, topology, ?TOPOLOGY),
    NumNodes = application:get_env(main, num_nodes, ?NUM_NODES),
    NodeTempFailureProb = application:get_env(main, temp_fail_prob, ?NODE_TEMP_FAILURE_PROB),
    NodePermFailureProb = application:get_env(main, perm_fail_prob, ?NODE_PERM_FAILURE_PROB),
    GridSize = application:get_env(main, grid_size, ?TOPOLOGY_GRID_SIZE),
    ConvergencePatience = application:get_env(main, patience, ?CONVERGENCE_PATIENCE),
    {ok, S} = file:open("run_1.csv", [append]),
    io:format("Starting ~p, ~p, ~p, ~p, ~p, ~p~n", [Problem, Topology, NumNodes,
                NodeTempFailureProb, NodePermFailureProb, GridSize]),
    single_run(
        S,
        [Problem, Topology, NumNodes, NodeTempFailureProb, NodePermFailureProb, GridSize, ConvergencePatience]
    ),
    {ok, self()}.

single_run(S, [Problem, Topology, NumNodes, TempFailProb,
            PermFailProb, GridSize, Patience]) ->
    if
        Problem == gossip ->
            gossip_api:start_link(Topology, Patience, TempFailProb, PermFailProb, ?MSG),
            spawn_gossipers(NumNodes),
            {converged, GracefulOrViolent, NumRounds} = spread_rumor(),
            io:format(S, "~p,~p,~p,~p,~p,~p,~p,~p~n", [Problem, Topology, NumNodes, TempFailProb, PermFailProb, GridSize, NumRounds, atom_to_list(GracefulOrViolent)]),
            io:format("Converged after ~p rounds in a ~p manner~n", [NumRounds, atom_to_list(GracefulOrViolent)]);
        Problem == push_sum ->
            push_sum_api:start_link(Topology, Patience, TempFailProb, PermFailProb),
            spawn_summers(NumNodes),
            push_sum_api:kickoff(),
            {converged, GracefulOrViolent, NumRounds} = push_sum(),
            io:format(S, "~p,~p,~p,~p,~p,~p,~p,~p~n", [Problem, Topology, NumNodes, TempFailProb, PermFailProb, GridSize, NumRounds, atom_to_list(GracefulOrViolent)]),
            io:format("Converged after ~p rounds in a ~p manner~n", [NumRounds, atom_to_list(GracefulOrViolent)])
    end.

push_sum() ->
    {Converged, GracefulOrViolent, RoundId} = push_sum_api:next_round(),
    case Converged of
        not_converged ->
            timer:sleep(5),
            push_sum();
        converged ->
            {converged, GracefulOrViolent, RoundId}
    end.

spread_rumor() ->
    {Converged, GracefulOrViolent, RoundId} = gossip_api:next_round(),
    case Converged of
        not_converged ->
            timer:sleep(5),
            spread_rumor();
        converged ->
            {converged, GracefulOrViolent, RoundId}
    end.

spawn_gossipers(0) ->
    ok;
spawn_gossipers(N) ->
    gossip_api:spawn_gossiper(),
    spawn_gossipers(N - 1).

spawn_summers(0) ->
    ok;
spawn_summers(N) ->
    push_sum_api:spawn_summer(),
    spawn_summers(N - 1).

terminate(_Reason, _State, _Data) ->
    init:stop().
