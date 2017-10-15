
# Edmonds Matching

This is an implementation of edmonds maximum unweighted matching
algorithm in haskell. The algorithm is also known as the Blossom
Algorithm. The specifications of the algorithm can be 
found in Combinatorial Optimization 2002, by Korte, Vygen 2002,
pages. 224-225.

## Description

The algorithm works on simple, unweighted, undirected graphs. It finds
a matching (a set of edges with no vertices in common) that is of
maximum size, ie. there does not exist a larger matching. 

### Runtime Bounds
According to the specification, the runtime is `O(n^3).` The runtime
on finding a matching for bipartite graphs is `O(nm)`. Here, n is the
number of vertices and m the number of edges.

## Heuristics

The implemnetation also features heuristics as an attempt to speed up
the algorithm:

1. Greedy Maximal Matching: Traverse the graph and greedily add edges
   to the matching, until not possible anymore.
   
2. Expand-Contract: Expand the graph so that it becomes bipartite,
   then find a maximum matching in the bipartite graph (which is much
   easier than in general graphs). Once a matching is found, contract
   the graph to it's original form, and repair the resulting matching.

## Runtime Plots

![Graphs with different number of vertices](image/vertices.png)
![Graphs with different number of edges](image/edges-1k-70k.png)
![Graphs with different number of edges](image/edges-100-25k.png)
![Heuristic matching size found](image/matching-found.png)
