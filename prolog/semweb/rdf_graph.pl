:- module(
  rdf_graph,
  [
    rdf_isomorphic_graphset/2, % +GraphSet1, +GraphSet2
    rdf_triples_graphset/2     % +Triples, -GraphSet
  ]
).

/** <module> RDF graph support

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(assoc)).
:- use_module(library(lists)).

:- use_module(library(semweb/rdf_term)).





%! rdf_isomorphic_graphsets(+GraphSet1:ordset(compound),
%!                          +GraphSet2:ordset(compound)) is semidet.
%
% Is true if there is a consistent mapping between of blank nodes in
% Graph1 to blank nodes in Graph2 that makes both graphs equal.  This
% maps to the Prolog notion of _variant_ if there was a canonical
% ordering of triples.
%
% Blank nodes are assumed to be replaced by Prolog variables.

rdf_isomorphic_graphset(GraphSet1, GraphSet2) :-
  once(graphset_permutation(GraphSet1, Perm1)),
  graphset_permutation(GraphSet2, Perm2),
  Perm1 =@= Perm2, !.

graphset_permutation(GraphSet, Graph) :-
  partition(ground, GraphSet, Ground, NonGround),
  permutation(NonGround, NonGroundPermutation),
  append(Ground, NonGroundPermutation, Graph).



%! rdf_triples_graphset(+Triples:list(compound),
%!                      -GraphSet:list(compound)) is det.

rdf_triples_graphset(Triples, GraphSet) :-
  rdf_triples_vars(Triples, Terms),
  sort(Terms, GraphSet).

rdf_triples_vars(Triples, Terms) :-
  empty_assoc(Map),
  rdf_triples_vars(Triples, Terms, Map, _).

rdf_triples_vars([], [], Map, Map).
rdf_triples_vars([rdf(S1,P,O1)|T1], [rdf(S2,P,O2)|T2], Map1, Map4) :-
  rdf_nonliteral_var(S1, S2, Map1, Map2),
  rdf_term_var(O1, O2, Map2, Map3),
  rdf_triples_vars(T1, T2, Map3, Map4).

rdf_nonliteral_var(BNode, Var, Map1, Map2) :-
  rdf_is_bnode(BNode), !,
  (   get_assoc(BNode, Map1, Var)
  ->  Map2 = Map1
  ;   put_assoc(BNode, Map1, Var, Map2)
  ).
rdf_nonliteral_var(Iri, Iri, Map, Map).

rdf_term_var(NonLiteral, Term, Map1, Map2) :-
  rdf_nonliteral_var(NonLiteral, Term, Map1, Map2), !.
rdf_term_var(Literal, Literal, Map, Map).


