:- module(
  rdf_stat,
  [
    count_datatype_triples/3, % ?Datatype:iri
                              % ?Graph:atom
                              % -NumberOfDistinctStatements:nonneg
    count_objects/4, % ?Subject:or([bnode,iri])
                     % ?Predicate:iri
                     % ?Graph:atom
                     % -NumberOfDistinctObjectTerms:nonneg
    count_predicates/4, % ?Subject:or([bnode,iri])
                        % ?Object:rdf_term
                        % ?Graph:atom
                        % -NumberOfDistinctPredicateTerms:nonneg
    count_subjects/4, % ?Predicate:iri
                      % ?Object:rdf_term
                      % ?Graph:atom
                      % -NumberOfDistinctSubjectTerms:nonneg
    count_triples/5 % ?Subject:or([bnode,iri])
                    % ?Predicate:iri
                    % ?Object:rdf_term
                    % ?Graph:atom
                    % -NumberOfDistinctStatements:nonneg
    description_size/2, % +Subject:or([bnode,iri])
                        % -NumberOfDistinctTriples:nonneg
    iris_by_graph/2, % ?Graph:atom
                     % -NumberOfDistinctIris:nonneg
  ]
).

/** <module> RDF statistics

Predicates for calculating simple statistics over RDF data.

@author Wouter Beek
@version 2013/01, 2013/03-2013/04, 2013/07, 2013/09, 2014/03, 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(graph/rdf_graph)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(count_datatype_triples(r,?,-)).
:- rdf_meta(count_objects(r,r,?,-)).
:- rdf_meta(count_predicates(r,r,?,-)).
:- rdf_meta(count_subjects(r,r,?,-)).
:- rdf_meta(count_triples(r,r,o,?,-)).
:- rdf_meta(description_size(r,-)).





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



%! count_objects(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Graph:atom,
%!   -NumberOfDistinctObjectTerms:nonneg
%! ) is det.
% Returns the number of distinct object terms for the given pattern.

count_objects(S, P, G, NumberOfDistinctObjectTerms):-
  aggregate_all(
    set(ObjectTerm),
    rdf(S, P, ObjectTerm, G),
    DistinctObjectTerms
  ),
  length(DistinctObjectTerms, NumberOfDistinctObjectTerms).



%! count_predicates(
%!   ?Subject:or([bnode,iri]),
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -NumberOfDistinctPredicateTerms:nonneg
%! ) is det.
% Returns the number of distinct predicate terms for the given patern.

count_predicates(S, O, G, NumberOfDistinctPredicateTerms):-
  aggregate_all(
    set(P),
    rdf(S, P, O, G),
    Ps
  ),
  length(Ps, NumberOfDistinctPredicateTerms).



%! count_subjects(
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?Graph:atom
%!   -NumberOfDistinctSubjectTerms:nonneg
%! ) is det.
% Returns the number of distinct subject terms for the given pattern.

count_subjects(P, O, G, NumberOfDistinctSubjectTerms):-
  aggregate_all(
    set(SubjectTerm),
    rdf(SubjectTerm, P, O, G),
    DistinctSubjectTerms
  ),
  length(DistinctSubjectTerms, NumberOfDistinctSubjectTerms).



%! count_triples(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -NumberOfDistinctStatements:nonneg
%! ) is det.

count_triples(S, P, O, G, NumberOfDistinctStatements):-
  aggregate_all(
    count,
    rdf(S, P, O, G),
    NumberOfDistinctStatements
  ).



%! description_size(
%!   +Subject:or([bnode,iri]),
%!   -NumberOfDistinctTriples:nonneg
%! ) is det.

description_size(S, N):-
  rdf_global_id(owl:sameAs, P),
  aggregate_all(
    sum(N0),
    (
      rdf_reachable(S, P, S0),
      count_triples(S0, _, _, _, N0)
    ),
    N
  ).



%! iris_by_graph(?Graph:atom, -NumberOfDistinctIris:nonneg) is det.
% Returns the numver of unique IRIs that occur in the given graph.

iris_by_graph(Graph, NumberOfDistinctIris):-
  aggregate_all(
    set(Iri),
    rdf_iri(Iri, Graph),
    DistinctIris
  ),
  length(DistinctIris, NumberOfDistinctIris).
