
# Edmonds Matching

This is an implementation of edmonds maximum unweighted matching
algorithm in haskell. The algorithm is also known as the Blossom
Algorithm. The specifications of the algorithm can be 
found in Combinatorial Optimization 2002, by Korte, Vygen 2002,
pages. 224-225.

**DISCLAIMER** This is by far a time-efficient implementation. It was
implemented in order to study the algorithm and compare some
heuristics.

## Running

Do `stack setup && stack build` to build the executable.
The program is run by `stack exec edmonds-matching -- HEURISTIC FILE`
where `HEURISTIC` is one if the strings `no|gm|ec` and `FILE` is the
path to a DIMACS-formated file. See example of this in `graphs`.


## Description

The algorithm works on simple, unweighted, undirected graphs. It finds
a matching (a set of edges with no vertices in common) that is of
maximum size, ie. there does not exist a larger matching. 

### Algorithm
The algorithm works by growing a datastructure called alternating
forest and augmenting root->leaf paths on it. The problem with general
graphs are that there might exist odd cycles, for which a node in the
alternating tree might be of even and odd distance from the root at
the same time. To overcome this problem, the algorithm "shrinks" these
cycles into a single vertex and then proceeds. It can be shown that
you don't "lose" anything by shrinking and unshrinking odd cycles. 

### Runtime Bounds
According to the specification, the runtime is `O(n^3).` The runtime
on finding a matching for bipartite graphs is `O(nm)`. Here, n is the
number of vertices and m the number of edges.

### Heuristics

The implemnetation also features heuristics as an attempt to speed up
the algorithm:

1. Greedy Maximal Matching: Traverse the graph and greedily add edges
   to the matching, until not possible anymore.
   
2. Expand-Contract: Expand the graph so that it becomes bipartite,
   then find a maximum matching in the bipartite graph (which is much
   easier than in general graphs). Once a matching is found, contract
   the graph to it's original form, and repair the resulting matching.
   
Edmonds Algorithm does not assume anything about the matching it starts
with. We can thus try to find a matching with one of these heuristics,
then load it into the algorithm and then start.

## Plots

### Relative runtimes using heuristics depending on denseness

![Graphs with different number of edges](image/edges-1k-70k.png)
![Graphs with different number of edges](image/edges-100-25k.png)

### Size of matching found by the two heuristics

![Heuristic matching size found](image/matching-found.png)
