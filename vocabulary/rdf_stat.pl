:- module(
  rdf_stat,
  [
    iris_by_graph/2, % ?Graph:atom
                     % -NumberOfDistinctIris:nonneg
    count_objects/4, % ?Subject:or([bnode,iri])
                     % ?Predicate:iri
                     % +Graph:atom
                     % -NumberOfDistinctObjectTerms:nonneg
    count_predicates/4, % ?Subject:or([bnode,iri])
                        % ?Object:rdf_term
                        % +Graph:atom
                        % -NumberOfDistinctPredicateTerms:nonneg
    count_subjects/4, % ?Predicate:iri
                      % ?Object:rdf_term
                      % ?Graph:atom
                      % -NumberOfDistinctSubjectTerms:nonneg
    rdf_triples_by_datatype/3, % ?Graph:atom
                               % ?Datatype:iri
                               % -NumberOfTriples:nonneg
    rdf_triples_by_object/3, % ?Graph:atom
                             % ?Object:rdf_term
                             % -NumberOfTriples:nonneg
    rdf_triples_by_pattern/5, % ?Subject:or([bnode,iri])
                              % ?Predicate:iri
                              % ?Object:rdf_term
                              % ?Graph:atom
                              % -NumberOfTriples:nonneg
    rdf_triples_by_predicate/3, % ?Graph:atom
                                % ?Predicate:iri
                                % -NumberOfTriples:nonneg
    rdf_triples_by_subject/3, % ?Graph:atom
                              % ?Subject:or([bnode,iri])
                              % -NumberOfTriples:nonneg
    rdf_triples_by_term/3 % ?Graph:atom
                          % ?RdfTerm:rdf_term
                          % -NumberOfTriples:nonneg
  ]
).

/** <module> RDF statistics

Statistics for RDF data.

@author Wouter Beek
@see Based on the definitions in section 4.6 of the VoID W3C specification,
     http://www.w3.org/TR/2011/NOTE-void-20110303/
@version 2013/01, 2013/03-2013/04, 2013/07, 2013/09, 2014/03, 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdfs)).

:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(graph/rdf_graph)).
:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(count_objects(r,r,+,-)).
:- rdf_meta(count_predicates(r,r,+,-)).
:- rdf_meta(count_subjects(r,r,+,-)).
:- rdf_meta(rdf_triples_by_datatype(?,r,-)).
:- rdf_meta(rdf_triples_by_object(?,o,-)).
:- rdf_meta(rdf_triples_by_pattern(r,r,o,?,-)).
:- rdf_meta(rdf_triples_by_predicate(?,r,-)).
:- rdf_meta(rdf_triples_by_subject(?,r,-)).
:- rdf_meta(rdf_triples_by_term(?,o,-)).
:- rdf_meta(triples_by_resource(r,-)).





%! iris_by_graph(?Graph:atom, -NumberOfDistinctIris:nonneg) is det.
% Returns the numver of unique IRIs that occur in the given graph.

iris_by_graph(Graph, NumberOfDistinctIris):-
  aggregate_all(
    set(Iri),
    rdf_iri(Iri, Graph),
    DistinctIris
  ),
  length(DistinctIris, NumberOfDistinctIris).



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



%! rdf_triples_by_datatype(
%!   ?Graph:atom,
%!   +Datatype:iri,
%!   -NumberOfTriples:nonneg
%! ) is det.

% For datatype IRI `rdf:langString`.
rdf_triples_by_datatype(G, D, ND):-
  rdf_equal(rdf:langString, D), !,
  rdf_triples_by_pattern(_, _, literal(lang(_,_)), G, ND).
% For all other datatype IRIs.
rdf_triples_by_datatype(G, D, ND):-
  rdf_triples_by_pattern(_, _, literal(type(D,_)), G, ND).



%! rdf_triples_by_object(
%!   ?Graph:atom,
%!   +Object:rdf_term,
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_object(G, O, NO):-
  rdf_triples_by_pattern(_, _, O, G, NO).



%! rdf_triples_by_pattern(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_pattern(S, P, O, G, N):-
  aggregate_all(count, rdf(S, P, O, G), N).



%! rdf_triples_by_predicate(
%!   ?Graph:atom,
%!   +Predicate:iri,
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_predicate(G, P, NP):-
  rdf_triples_by_pattern(_, P, _, G, NP).



%! triples_by_resource(+Resource:iri, -N:nonneg) is det.

triples_by_resource(Resource, N):-
  rdf_global_id(owl:sameAs, P),
  aggregate_all(
    count,
    (
      rdf_reachable(Resource, P, Resource0),
      rdf(Resource0, _, _)
    ),
    N
  ).



%! rdf_triples_by_subject(
%!   ?Graph:atom,
%!   +Subject:or([bnode,iri]),
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_subject(G, S, NS):-
  rdf_triples_by_pattern(S, _, _, G, NS).



%! rdf_triples_by_term(
%!   ?Graph:atom,
%!   ?RdfTerm:rdf_term,
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_term(G, T, N):-
  rdf_triples_by_subject(  G, T, NS),
  rdf_triples_by_predicate(G, T, NP),
  rdf_triples_by_object(   G, T, NO),
  rdf_triples_by_datatype( G, T, ND),
  sum_list([NS,NP,NO,ND], N).

