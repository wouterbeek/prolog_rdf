:- module(
  rdf_triples,
  [
    rdf_triples/2 % +Graph:atom
                  % -Triples:ordset(compound)
    rdf_triples_edges/2, % +Triples:list(compound)
                         % -Edges:ordset(pair(rdf_term))
    rdf_triples_vertices/2, % +Triples:list(compound)
                            % -Vertices:ordset(rdf_term)
  ]
).

/** <module> RDF triples

Support for RDF triple compound terms.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(lists, except([delete/3]))).
:- use_module(library(semweb/rdf_db)).



%! rdf_triples(+Graph:atom, -Triples:ordset(compound)) is det.

rdf_triples(Graph, Triples):-
  aggregate_all(
    set(rdf(S,P,O),
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
