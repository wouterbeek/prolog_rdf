:- module(
  hdt_stmt,
  [
    hdt/3, % ?S, ?P, ?O
    hdt/4  % ?S, ?P, ?O, ?G
  ]
).

/** <module> HDT statements API

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(hdt), []).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   hdt(r, r, o),
   hdt(r, r, o, r).





%! hdt(?S, ?P, ?O) is nondet.
%! hdt(?S, ?P, ?O, ?G) is nondet.

hdt(S, P, O) :-
  distinct(rdf(S,P,O), hdt(S, P, O, _)).


hdt(S, P, O, G) :-
  q_fs:hdt_graph(G, Hdt),
  hdt:hdt_search(Hdt, S, P, O).
