-module(topology).
-behaviour(gen_server).

-include("../include/hyperparams.hrl").
-compile([export_all]).


get_neighbor(Name, TopologyType, NodeId, Nodes) ->
    gen_server:call({global, Name}, {get_neighbor, TopologyType, NodeId, Nodes}).

start_link(Name) ->
    gen_server:start_link({global, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, {}}.


two_to_one({X, Y}) ->
    % Convert 2D to 1D coordinate.
    X + ?TOPOLOGY_GRID_SIZE * Y.

one_to_two(I) ->
    % Convert 1D to 2D coordinates.
    {I rem ?TOPOLOGY_GRID_SIZE, I div ?TOPOLOGY_GRID_SIZE}.

get_2d_neighbors(List, Idx) ->
    {X0, Y0} = one_to_two(Idx),
    Neighbors = lists:map(
        fun(P) ->
            two_to_one(P)
        end,
        [
            {X0, Y0 - 1},
            {X0, Y0 + 1},
            {X0 - 1, Y0},
            {X0 + 1, Y0}
        ]
    ),
    lists:filtermap(
        fun(I) ->
            if
                (I < 1) or (I > length(List)) ->
                    false;
                true ->
                    {true, lists:nth(I, List)}
            end
        end,
        Neighbors
    ).


one_to_three(I) ->
    {
        I div (?TOPOLOGY_GRID_SIZE * ?TOPOLOGY_GRID_SIZE),
        (I div ?TOPOLOGY_GRID_SIZE) rem ?TOPOLOGY_GRID_SIZE,
        I rem ?TOPOLOGY_GRID_SIZE
    }.

three_to_one({X, Y, Z}) ->
    X + (Y * ?TOPOLOGY_GRID_SIZE) + (Z * ?TOPOLOGY_GRID_SIZE * ?TOPOLOGY_GRID_SIZE).

get_3d_neighbors(List, Idx) ->
    {X0, Y0, Z0} = one_to_three(Idx),
    Neighbors = lists:map(
        fun(P) ->
            three_to_one(P)
        end,
        [
            {X0, Y0, Z0 + 1},
            {X0, Y0, Z0 - 1},
            {X0, Y0 + 1, Z0},
            {X0, Y0 - 1, Z0},
            {X0 + 1, Y0, Z0},
            {X0 - 1, Y0, Z0}
        ]
    ),
    lists:filtermap(
        fun(I) ->
            if
                (I < 1) or (I > length(List)) ->
                    false;
                true ->
                    {true, lists:nth(I, List)}
            end
        end,
        Neighbors
    ).


handle_call({get_neighbor, fully_connected, NodeId, Nodes}, _From, State) ->
    % Choose a random neighbor that is not the same as the current node.
    Cands = lists:delete(
        NodeId,
        lists:seq(1, length(Nodes))
    ),
    NeighborId = lists:nth(rand:uniform(length(Cands)), Cands),
    RandNeighbor = lists:nth(NeighborId, Nodes),
    {reply, RandNeighbor, State};

handle_call({get_neighbor, grid, NodeId, Nodes}, _From, State) ->
    % Deterministically wrap around the list of PIDs in the
    % shape of a 2D grid and return the 2D neighbor.
    Neighbors = get_2d_neighbors(Nodes, NodeId),
    RandNeighbor = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
    {reply, RandNeighbor, State};

handle_call({get_neighbor, line, NodeId, Nodes}, _From, State) ->
    case NodeId of
        N when N == 1 ->
            NeighborIndices = [2];
        N when N == length(Nodes) ->
            NeighborIndices = [length(Nodes) - 1];
        _ ->
            NeighborIndices = [NodeId - 1, NodeId + 1]
    end,
    RandNeighborIdx = lists:nth(rand:uniform(length(NeighborIndices)), NeighborIndices),
    RandNeighbor = lists:nth(RandNeighborIdx, Nodes),
    {reply, RandNeighbor, State};

handle_call({get_neighbor, imperfect_grid, NodeId, Nodes}, _From, State) ->
    % Include an random, "imperfect" neighbor in the list of potential neighbors.
    ImperfectNeighbor = lists:nth(rand:uniform(length(Nodes)), Nodes),
    Neighbors = get_3d_neighbors(Nodes, NodeId) ++ [ImperfectNeighbor],
    RandNeighbor = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
    {reply, RandNeighbor, State}.

handle_info(_Info, State) ->
    {noreply, State}.
