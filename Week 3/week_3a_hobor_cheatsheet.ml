(* Aquinas Hobor *)
(* Class 3A *)

(* This file has no code because we introduced the following topics on 
the board, which I've nicely summarized here for you:

A) logs: key rules

log base b is written log_b; when b is omitted (usual) it is 2

log 2 = 1
log_b x = (log x) / (log b)
log (a * b) = (log a) + (log b)
log (a^b) = b * (log a)
log (2^x) = x

B) big-O notation

Definition: an algorithm with input size n and running time g(n) 

is called

O(f(n)) when:

1) after some initial period n', for all values n'' > n',
2) there exists some constant k, such that,
3) g(n'') <= k * f(n)

Thus, f(n) is an upper bound on the running time -- that is, the algorithm will take no more than f(n) time (maybe times a constant).

Some big-O intutition:

1) Take only the largest additive term
2) Drop all *constant* multiplicative factors

Examples of common big-O classes:

1) O(1) -- used for all constant-time operations
2) O(log n) -- used for logarithmic-time functions
3) O(n) -- linear time
4) O(n * log n) -- so-called quasilinear time
5) O(n^2) -- quadratic time
6) O(2^n) -- exponential time

We've seen several examples of some of these so far, such as:

1) All nonrecursive functions (that don't call other functions) are O(1)
2) Finding the min and max of a list is linear time (in the length of the list)
3) Calculating the exponential the slow way (repeated multiplication) is linear time in the the magnitude of the exponent
4) Calculating the exponential the fast way (repeated squaring) is logarithmic time in the magnitude of the exponent
5) Calculating the Fibonacci function the slow way is exponential time in the magnitude of the input -- at least big-omega(2^(n/2))
6) Calculating the Fibonacci function the fast way is linear time in the magnitude of the input

 *)
