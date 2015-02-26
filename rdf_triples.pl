:- module(
  rdf_triples,
  [
    rdf_ground_triple/4, % ?Subject:or([bnode,iri])
                         % ?Predicate:iri
                         % ?Object:rdf_term
                         % ?Graph:atom
    rdf_is_ground_triple/1, % @Term
    rdf_is_object/1, % @Term
    rdf_is_predicate/1, % @Term
    rdf_is_subject/1, % @Term
    rdf_is_triple/1, % @Triple
    rdf_triples/2, % +Graph:atom
                   % -Triples:ordset(compound)
    rdf_triples_to_edges/2, % +Triples:list(compound)
                            % -Edges:ordset(pair(rdf_term))
    rdf_triples_to_terms/2 % +Triples:list(compound)
                           % -Vertices:ordset(rdf_term)
  ]
).

/** <module> RDF triples

Support for RDF triple compound terms.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_ground_triple(r,r,o,?)).
:- rdf_meta(rdf_is_ground_triple(t)).
:- rdf_meta(rdf_is_triple(t)).





%! rdf_ground_triple(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.

rdf_ground_triple(S, P, O, G):-
  rdf(S, P, O, G),
  rdf_is_ground_triple(rdf(S,P,O)).



%! rdf_is_ground_triple(@Term) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.

rdf_is_ground_triple(rdf(S,P,O)):-
  rdf_is_name(S),
  rdf_is_resource(P),
  rdf_is_name(O).



%! rdf_is_object(@Term) is semidet.

rdf_is_object(Term):-
  rdf_is_subject(Term).
rdf_is_object(Term):-
  rdf_is_literal(Term).



%! rdf_is_predicate(@Term) is semidet.

rdf_is_predicate(Term):-
  rdf_is_resource(Term).



%! rdf_is_subject(@Term) is semidet.

rdf_is_subject(Term):-
  rdf_is_bnode(Term).
rdf_is_subject(Term):-
  rdf_is_resource(Term).



%! rdf_is_triple(@Term) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.

rdf_is_triple(rdf(S,P,O)):-
  rdf_is_subject(S),
  rdf_is_predicate(P),
  rdf_is_object(O).



%! rdf_triple_to_term(+Triple:compound, -Term:rdf_term) is nondet.

rdf_triple_to_term(rdf(S,_,_), S).
rdf_triple_to_term(rdf(_,P,_), P).
rdf_triple_to_term(rdf(_,_,O), O).



%! rdf_triples(+Graph:atom, -Triples:ordset(compound)) is det.

rdf_triples(Graph, Triples):-
  aggregate_all(
    set(rdf(S,P,O)),
    rdf(S, P, O, Graph),
    Triples
  ).



%! rdf_triples_to_edges(
%!   +Triples:list(rdf_term),
%!   -Edges:ordset(compound)
%! ) is det.

rdf_triples_to_edges(Ts, Es):-
  aggregate_all(
    set(FromV-ToV),
    member(rdf(FromV,_,ToV), Ts),
    Es
  ).



%! rdf_triples_to_terms(
%!   +Triples:list(rdf_term),
%!   -Terms:ordset(rdf_term)
%! ) is det.

rdf_triples_to_terms(Triples, Terms):-
  aggregate_all(
    set(Term),
    (
      member(Triple, Triples),
      rdf_triple_to_term(Triple, Term)
    ),
    Terms
  ).
