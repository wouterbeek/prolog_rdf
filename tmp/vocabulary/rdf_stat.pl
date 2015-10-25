:- module(
  rdf_stat,
  [
    count_datatype_triples/3, % ?Datatype:iri
                              % ?Graph:atom
                              % -NumberOfDistinctStatements:nonneg
    iris_by_graph/2 % ?Graph:atom
                    % -NumberOfDistinctIris:nonneg
  ]
).

/** <module> RDF statistics

Predicates for calculating simple statistics over RDF data.

@author Wouter Beek
@version 2013/01, 2013/03-2013/04, 2013/07, 2013/09, 2014/03, 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(count_datatype_triples(r,?,-)).


%! count_datatype_triples(
%!   +Datatype:iri,
%!   ?Graph:atom,
%!   -NumberOfDistinctStatements:nonneg
%! ) is det.

% For datatype IRI `rdf:langString`.
count_datatype_triples(rdf:langString, Graph, N):- !,
  count_triples(_, _, literal(lang(_,_)), Graph, N).
% For all other datatype IRIs.
count_datatype_triples(Datatype, Graph, N):-
  count_triples(_, _, literal(type(Datatype,_)), Graph, N).


%! iris_by_graph(?Graph:atom, -NumberOfDistinctIris:nonneg) is det.
% Returns the numver of unique IRIs that occur in the given graph.

iris_by_graph(G, NXs):-
  aggregate_all(
    count,
    (user:rdf_iri(X), rdf_term(X, G)),
    NXs
  ).
