import os
import subprocess
from itertools import product
from multiprocessing import Pool

PREFIX = "./gossip_release/erts-13.0.4/bin/erl -boot ./gossip_release/releases/1.0.0/start -noshell -main "

problems = ["push_sum"]
num_nodes = [10, 20, 50, 75, 100, 250, 500] #, 5000, 10000, 50000]
topologies = ["fully_connected", "grid", "line", "imperfect_grid"]
grid_sizes = [10]
temp_fail_probs = [0] #, 0.1, 0.5]
perm_fail_probs = [0] #, 0.1, 0.5]

with open("run_1.csv", "w") as f:
    f.write("Problem,Topology,NumNodes,TempFailProb,PermFailProb,GridSize,ItersToConverge\n")

for problem, num_node, topology, grid_size, temp_fail_prob, perm_fail_prob in product(problems, num_nodes, topologies, grid_sizes, temp_fail_probs, perm_fail_probs):
    os.system(" ".join([
        "bash", PREFIX,
        "problem", problem,
        "topology", topology,
        "num_nodes", str(num_node),
        "temp_fail_prob", str(temp_fail_prob),
        "perm_fail_prob", str(perm_fail_prob),
        "grid_size", str(grid_size)
    ]) + " &")
