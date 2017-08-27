# Idea

Ignore cycles, act if bypirtate => Can lead to some bad befaviour. Fix it.

# Steps

## (1) Transform graph into B-graph. (bipartite)

forall. $v in V(G) create $va, $vb

forall. {$v, $w} in E(G) create {$va, $wb}, {$wa, $vb}

## (2) Calculate a maximum matching

As in the lecture

## (3) Transform B-graph back to normal

forall. {$vx, $wy} in M let {$v, $w} in M

## (4) Repair

Look at G' = (V(G), M)

(a) Find all paths of matchings of even length and repair them (remove every
second edge (Cannot exist any path of odd length)

(b) Find all cycles of even length and repair them

(c) Find all cycles odd length and repair them. (Remove every second
edge.)

OBSERVE: Must take care of paths first.

# Notes

Probably better for dense graphs

Comparison with greedy maximal matching, Runtime and size difference =>
Eventual affect on Edmonds

