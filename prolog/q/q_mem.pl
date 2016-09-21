:- module(
  q_mem,
  [
    q_mem/3 % +M, ?G, +Goal_0
  ]
).

/** <module> Quine memoization

Currently only supported for backend `trp`.

@author Wouter Beek
@version 2016/09
*/

:- use_module(library(semweb/rdf11)).

:- dynamic
    q_mem_cache/2.

:- meta_predicate
    q_mem(+, ?, 0).

:- rdf_meta
   q_mem(+, r, :).





%! q_mem(+M, ?G, +Goal_0) is det.

q_mem(_, G, Goal_0) :-
  var(G), !,
  call(Goal_0).
q_mem(trp, G, Goal_0) :-
  rdf_graph_property(G, hash(Hash)),
  (   q_mem_cache(Hash, Goal_0)
  ->  true
  ;   retractall(q_mem_cache(Hash, Goal_0)),
      call(Goal_0),
      assert(q_mem_cache(Hash, Goal_0))
  ).
q_mem(_, _, Goal_0) :-
  call(Goal_0).
