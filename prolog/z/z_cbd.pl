:- module(
  z_cbd,
  [
    z_cbd/2,         % ?Node,     -Triples
    z_cbd/3,         % ?Node, ?G, -Triples
    z_cbd_triple/2,  % +Node,     -Triple
    z_cbd_triple/3,  % +Node, ?G, -Triple
    z_scbd/2,        % ?Node,     -Triples
    z_scbd/3,        % ?Node, ?G, -Triples
    z_scbd_triple/2, % +Node,     -Triple
    z_scbd_triple/3  % +Node, ?G, -Triple
  ]
).

/** <module> Z (S)CBD

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(z/z_stmt)).
:- use_module(library(z/z_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).

:- rdf_meta
   z_cbd(o, -),
   z_cbd(o, r, -),
   z_cbd_triple(o, -),
   z_cbd_triple(o, r, -),
   z_scbd(o, -),
   z_scbd(o, r, -),
   z_scbd_triple(o, -),
   z_scbd_triple(o, r, -).





%! z_cbd(?Node, -Triples) is det.
%! z_cbd(?Node, ?G, -Triples) is det.
%! z_cbd_triple(+Node, -Triple) is nondet.
%! z_cbd_triple(+Node, ?G, -Triple) is nondet.

z_cbd(Node, Triples) :-
  z_cbd(Node, _, Triples).


z_cbd(Node, G, Triples) :-
  z_subject(Node, G),
  aggregate_all(set(Triple), z_cbd_triple0(Node, G, Triple), Triples).


z_cbd_triple(Node, Triple) :-
  z_cbd_triple(Node, _, Triple).


z_cbd_triple(Node, G, Triple) :-
  distinct(Triple, z_cbd_triple0(Node, G, Triple)).


z_cbd_triple0(S, G, Triple) :-
  z(S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   z_is_bnode(O),
      z_cbd_triple0(O, G, Triple)
  ;   z_reification(S, P, O, Stmt),
      z_cbd_triple0(Stmt, G, Triple)
  ).



%! z_scbd(?Node, -Triples) is det.
%! z_scbd(?Node, ?G, -Triples) is det.
%! z_scbd_triple(+Node, -Triple) is nondet.
%! z_scbd_triple(+Node, ?G, -Triple) is nondet.

z_scbd(Node, Triples) :-
  z_scbd(Node, _, Triples).


z_scbd(Node, G, Triples) :-
  z_term(Node, G),
  aggregate_all(set(Triple), z_scbd_triple0(Node, G, Triple), Triples).


z_scbd_triple(Node, Triple) :-
  z_scbd_triple(Node, _, Triple).


z_scbd_triple(Node, G, Triple) :-
  distinct(Triple, z_scbd_triple0(Node, G, Triple)).


z_scbd_triple0(O, G, Triple) :-
  z_cbd_inv_triple0(O, G, Triple).
z_scbd_triple0(S, G, Triple) :-
  z_cbd_triple0(S, G, Triple).


z_cbd_inv_triple0(O, G, Triple) :-
  z(S, P, O, G),
  (   Triple = rdf(S,P,O)
  ;   z_is_bnode(S),
      z_cbd_inv_triple0(S, G, Triple)
  ;   z_reification(S, P, O, G, Stmt),
      z_scbd_triple0(Stmt, G, Triple)
  ).
