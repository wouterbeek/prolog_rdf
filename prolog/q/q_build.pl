:- module(
  q_build,
  [
    q_assert/2, % +M, +Tuple
    q_assert/3, % +M, +Triple, +G
    q_assert/4, % +M, +S, +P, +O
    q_assert/5  % +M, +S, +P, +O, +G
  ]
).

/** <module> Quine build API

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(gen/gen_ntuples)).
:- use_module(library(q/q__io)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(zlib)).

:- rdf_meta
   q_assert(+, t),
   q_assert(+, t, r),
   q_assert(+, r, r, o),
   q_assert(+, r, r, o, r).





%! q_assert(+M, +Tuple) is det.
%! q_assert(+M, +Triple, +G) is det.
%! q_assert(+M, +S, +P, +O) is det.
%! q_assert(+M, +S, +P, +O, +G) is det.

q_assert(M, rdf(S,P,O)) :- !,
  q_assert(M, S, P, O).
q_assert(M, rdf(S,P,O,G)) :-
  q_assert(M, S, P, O, G).


q_assert(M, rdf(S,P,O), G) :-
  q_assert(M, S, P, O, G).


q_assert(M, S, P, O) :-
  q_default_graph(G),
  q_assert(M, S, P, O, G).


q_assert(hdt, S, P, O, G) :- !,
  hdt__assert(S, P, O, G).
q_assert(rdf, S, P, O, G) :- !,
  rdf_assert(S, P, O, G).
