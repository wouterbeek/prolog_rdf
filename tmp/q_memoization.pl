:- module(
  q_memoization,
  [
    q_memoization/3 % +M, ?G, +Goal_0
  ]
).

/** <module> Quine memoization

Currently only supported for backend `trp`.

@author Wouter Beek
@version 2016/09
*/

:- use_module(library(semweb/rdf11)).

:- dynamic
    q_memoization_cache/2.

:- meta_predicate
    q_memoization(+, ?, 0).

:- rdf_meta
   q_memoization(+, r, :).





%! q_memoization(+M, ?G, +Goal_0) is det.

q_memoization(_, G, Goal_0) :-
  var(G), !,
  call(Goal_0).
q_memoization(trp, G, Goal_0) :-
  rdf_graph_property(G, hash(Hash)),
  (   q_memoization_cache(Hash, Goal_0)
  ->  true
  ;   retractall(q_memoization_cache(Hash, Goal_0)),
      call(Goal_0),
      assert(q_memoization_cache(Hash, Goal_0))
  ).
q_memoization(_, _, Goal_0) :-
  call(Goal_0).
