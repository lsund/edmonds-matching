# Post-project description

## Objective

## What was implemented under which specification

### Algoritm

Edmonds Cardinality Matching Algorithm (The blossom algorithm) 
implemented exactly according to to Korte, Vygen 2002, pg. 224-225.

### Heuristics:
* GreedyMaximal implemented according to own specification
* ExpandContract implemented according to Adrians specification

## Theoretical running time

### Edmonds cardinality matching algorithm
According to THM 10.31, the algorithm determines a maximum matching in time 
O(n^3) where n = |V(G)|

### Bipartite matching
According to TM 10.5, bipartite matching can be solved in O(mn) as a 
maximum integral s-t flow can be seen as a maximum cardinality matching.

Since I'm using the specified algorithm also for the bipartite case, I can't 
get a better bound on the runtime for the bipartie case.

## Actual runtime/Analysis

* Plot: Algorithm runtime comparison with increasing edge-number

* Plot: Algorithm runtime comparison with increasing vertex-number

* Some chosen special graphs (maybe)

## Difficulties

### Understanding the algorithm

The algorithm as specified works by iterating over and updating three special
vectors mu, ro and phi, which describes the alternating tree which are grown 
in each iteration.

* We can use mu and phi to traverse the tree from the root and ro to specify 
  the blossoms

### Choice of data-structures

### Shortcomings

## Programming language

### Pros

### Cons

### Usage of built-in libraries

## Conclusion

