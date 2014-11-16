:- module(
  rdf_triple,
  [
    rdf_ground_triple/4, % ?Subject:or([bnode,iri])
                         % ?Predicate:iri
                         % ?Object:rdf_term
                         % ?Graph:atom
    rdf_is_ground_triple/1, % @Term
    rdf_is_triple/1, % @Triple
    rdf_triples/2, % +Graph:atom
                   % -Triples:ordset(compound)
    rdf_triples_edges/2, % +Triples:list(compound)
                         % -Edges:ordset(pair(rdf_term))
    rdf_triples_vertices/2 % +Triples:list(compound)
												   % -Vertices:ordset(rdf_term)
  ]
).

/** <module> RDF triple

Support for RDF triple compound terms.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(semweb/rdf_db)).

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

rdf_is_ground_triple(rdf(S,_,O)):-
  rdf_is_name(S),
  rdf_is_resource(P),
  rdf_is_name(O).



%! rdf_is_triple(@Term) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.

rdf_is_triple(rdf(S,P,O)):-
  rdf_is_subject(S),
  rdf_is_predicate(P),
  rdf_is_object(O).



%! rdf_triples(+Graph:atom, -Triples:ordset(compound)) is det.

rdf_triples(Graph, Triples):-
  aggregate_all(
    set(rdf(S,P,O)),
    rdf(S, P, O, Graph),
    Triples
  ).



%! rdf_triples_edges(
%!   +Triples:list(or([bnode,iri,literal])),
%!   -Edges:ordset(compound)
%! ) is det.

rdf_triples_edges(Ts, Es):-
  aggregate_all(
    set(FromV-ToV),
    member(rdf(FromV,_,ToV), Ts),
    Es
  ).



%! rdf_triples_vertices(
%!   +Triples:list(or([bnode,iri,literal])),
%!   -Vertices:ordset(or([bnode,iri,literal]))
%! ) is det.

rdf_triples_vertices(Ts, Vs):-
  aggregate_all(
    set(V),
    (
      member(rdf(V1,_,V2), Ts),
      (   V = V1
      ;   V = V2
      )
    ),
    Vs
  ).
