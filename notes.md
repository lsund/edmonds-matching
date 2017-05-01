
### Data Structures
To store a special blossom forest F, we introduce the following data
structures:
For each vertex x we have three variables mu(x), phi(x), ro(x) with the
following properties:

mu x = x, if x is not covered by M
mu x = y, if x is covered by edge (x,y) in M

phi(x) = x, if x is not in the forest or x is the base of a blossom in F
phi(x) = y, if x is an inner vertex and (x,y) is an unmatched edge in the
              forest
phi(x) = y, if x is an outer vertex and (x,y) is an unmatched edge
              according to a M-alternating ear-decomposition of the blossom
              containing x.


ro(x) = x if x is not an outer vertex
ro(x) = y if x is an outer vertex and y is the base of the outer blossom in F
               containing x


mu(x) is itself if not covered or neighbour if covered
phi(x) is ...
ro(x) is...


P(v) = v, mu(v), phi(mu(v)), mu(phi(mu(v))), phi(mu(phi(mu(v)))) ...

### Properties
Properties: 
(1) For each outer vertex v, P(v) is a v-q alternating path where q
is the root of the tree containing v.

(2) A vertex x is 
    * Outer iff mu(x) = x  or phi(mu(x)) \= mu(x)
    * Inner iff phi(mu(x)) == mu(x) and phi(x) \= x
    * out-of-forest iff mu(x) \= x and phi(x) == x and phi(mu(x)) == mu(x)
