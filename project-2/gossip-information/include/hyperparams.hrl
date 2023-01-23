-define(NUM_NODES, 100).
-define(TOPOLOGY, fully_connected). % fully_connected | grid | line | imperfect_grid
-define(TOPOLOGY_GRID_SIZE, 5). % For simplicity, we use the same dimension for all three axes (X, Y, and Z).
-define(NODE_TEMP_FAILURE_PROB, 0.0).
-define(NODE_PERM_FAILURE_PROB, 0.0).

% Number of rounds passed with no changes to the network after which
% the network is declared as "converged".
-define(CONVERGENCE_PATIENCE, 5).

-define(PROBLEM, push_sum). % gossip | push_sum

-define(MSG, "the queen is dead"). % Gossip message
