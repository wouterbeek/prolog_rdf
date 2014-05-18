:- module(
  rdf_stat,
  [
    count_classes/2, % +Graph:atom
                     % -Count:nonneg
    count_entities/2, % +Graph:atom
                      % -Count:nonneg
    count_individuals/3, % +Class:iri
                         % +Graph:atom
                         % -NumberOfIndividuals:nonneg
    count_objects/4, % ?Subject:or([bnode,iri])
                     % ?Predicate:iri
                     % +Graph:atom
                     % -Count:nonneg
    count_properties/4, % ?Subject:or([bnode,iri])
                        % ?Object:or([bnode,iri,literal])
                        % +Graph:atom
                        % -Count:nonneg
    count_subjects/4, % ?Predicate:iri
                      % ?Object:or([bnode,iri,literal])
                      % +Graph:atom
                      % -Count:nonneg
    rdf_property_table/3, % +Property:iri
                          % +Graph:atom
                          % -Table:list(list)
    rdf_triples_by_datatype/3, % ?RdfGraph:atom
                               % ?Datatype:iri
                               % -NumberOfTriples:nonneg
    rdf_triples_by_object/3, % ?RdfGraph:atom
                             % ?Object:or([bnode,iri,literal])
                             % -NumberOfTriples:nonneg
    rdf_triples_by_pattern/5, % ?Subject:or([bnode,iri])
                              % ?Predicate:iri
                              % ?Object:or([bnode,iri,literal])
                              % ?RdfGraph:atom
                              % -NumberOfTriples:nonneg
    rdf_triples_by_predicate/3, % ?RdfGraph:atom
                                % ?Predicate:iri
                                % -NumberOfTriples:nonneg
    rdf_triples_by_subject/3, % ?RdfGraph:atom
                              % ?Subject:or([bnode,iri])
                              % -NumberOfTriples:nonneg
    rdf_triples_by_term/3 % ?RdfGraph:atom
                          % ?RdfTerm:or([bnode,iri,literal])
                          % -NumberOfTriples:nonneg
  ]
).

/** <module> RDF statistics

Statistics for RDF data.

@author Wouter Beek
@see Based on the definitions in section 4.6 of the VoID W3C specification,
     http://www.w3.org/TR/2011/NOTE-void-20110303/
@version 2013/01, 2013/03-2013/04, 2013/07, 2013/09, 2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf_term(rdf_term)).
:- use_module(rdfs(rdfs_read)).

:- rdf_meta(count_individuals(r,+,-)).
:- rdf_meta(count_objects(r,r,+,-)).
:- rdf_meta(count_properties(r,r,+,-)).
:- rdf_meta(count_subjects(r,r,+,-)).
:- rdf_meta(rdf_property_table(r,+,-)).
:- rdf_meta(rdf_triples_by_datatype(?,r,-)).
:- rdf_meta(rdf_triples_by_object(?,o,-)).
:- rdf_meta(rdf_triples_by_pattern(r,r,o,?,-)).
:- rdf_meta(rdf_triples_by_predicate(?,r,-)).
:- rdf_meta(rdf_triples_by_subject(?,r,-)).
:- rdf_meta(rdf_triples_by_term(?,o,-)).



%! count_classes(+Graph:atom, -Count:nonneg) is det.
% Returns the number of distinct classes in the given graph.
%
% The total number of distinct classes in the graph.
% In other words, the number of distinct class URIs
% occuring as objects of =|rdf:type|= triples in the graph.
%
% This definition is actually incorrect, since a class can be specified
% using either of the following triple schemes, without the class having
% to occur in the object position of any triple.
% ~~~
% <C,  rdf:type,        rdfs:Class>
% <C, rdfs:subClassOf,  C'        >
% ~~~
%
% @see Based on the definition of =|void:classes|=.

count_classes(G, Count):-
  aggregate_all(
    set(O),
    rdf(_, rdf:type, O, G),
    Os
  ),
  length(Os, Count).

%! count_entities(+Graph:atom, -Count:nonneg) is det.
% Returns the numver of unique entities in the given graph.
%
%	The total number of entities that are described in the graph.
% To be an entity in a graph, a resource must have a URI.
%
% @see Based on the definition of =|void:entities|=.

count_entities(G, Count):-
  aggregate_all(
    set(E),
    rdf_iri(E, G),
    Es
  ),
  length(Es, Count).

%! count_individuals(
%!   +Class:iri,
%!   +Graph:atom,
%!   -NumberOfIndividuals:nonneg
%! ) is det.

count_individuals(C, G, N):-
  aggregate_all(
    set(I),
    rdfs_individual(m(t,f,f), I, C, G),
    Is
  ),
  length(Is, N).

%! count_objects(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   +Graph:atom,
%!   -Count:nonneg
%! ) is det.
% Returns the number of distinct objects
%
% The total number of distinct objects in the graph.
% In other words, the number of distinct URIs, blank nodes, or literals
% that occur in the object position of triples in the graph.
%
% @see Based on the definition of =|void:distinctObjects|=.

count_objects(S, P, G, Count):-
  aggregate_all(
    set(O),
    rdf(S, P, O, G),
    Os
  ),
  length(Os, Count).

%! count_properties(
%!   ?Subject:or([bnode,iri]),
%!   ?Object:or([bnode,iri,literal]),
%!   +Graph:atom,
%!   -Count:nonneg
%! ) is det.
% Returns the number of distinct properties in the given graph.
%
% The total number of distinct properties in the graph.
% In other words, the number of distinct property URIs
% that occur in the predicate position of triples in the graph.
%
% This definition is actually incorrect, since a property can be specified
% using either of the following triple schemes, without the property having
% to occur in the predicate position of any triple.
% ~~~
% <P,  rdf:type,           rdf:Property>
% <P1, rdfs:subPropertyOf, P2          >
% ~~~
%
% @see Based on the definition of =|void:properties|=.

count_properties(S, O, G, Count):-
  aggregate_all(
    set(P),
    rdf(S, P, O, G),
    Ps
  ),
  length(Ps, Count).

%! count_subjects(
%!   +Predicate:iri,
%!   +Object:or([bnode,literal,iri]),
%!   +Graph:atom
%!   -Count:nonneg
%! ) is det.
% Returns the number of unique subject terms that occur in triples
% with the given predicate-object pair and in the given graph.
%
% The total number of distinct subjects in the graph.
% In other words, the number of distinct URIs or blank nodes
% that occur in the subject position of triples in the graph.
%
% @see Based on the definition of =|void:distinctSubjects|=.

count_subjects(P, O, G, Count):-
  aggregate_all(
    set(S),
    rdf(S, P, O, G),
    Ss
  ),
  length(Ss, Count).

rdf_property_table(P, G, T):-
  aggregate_all(
    set(O),
    rdf([graph_mode(no_index)], _, P, O, G),
    Os
  ),
  findall(
    [O,NumberOfO],
    (
      member(O, Os),
      aggregate_all(
        count,
        rdf([graph_mode(no_index)], _, P, O, G),
        NumberOfO
      )
    ),
    T
  ).


%! rdf_triples_by_datatype(
%!   ?RdfGraph:atom,
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
%!   ?RdfGraph:atom,
%!   +Object:or([bnode,iri,literal]),
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_object(G, O, NO):-
  rdf_triples_by_pattern(_, _, O, G, NO).


%! rdf_triples_by_pattern(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?RdfGraph:atom,
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_pattern(S, P, O, G, N):-
  aggregate_all(count, rdf(S, P, O, G), N).


%! rdf_triples_by_predicate(
%!   ?RdfGraph:atom,
%!   +Predicate:iri,
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_predicate(G, P, NP):-
  rdf_triples_by_pattern(_, P, _, G, NP).


%! rdf_triples_by_subject(
%!   ?RdfGraph:atom,
%!   +Subject:or([bnode,iri]),
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_subject(G, S, NS):-
  rdf_triples_by_pattern(S, _, _, G, NS).


%! rdf_triples_by_term(
%!   ?RdfGraph:atom,
%!   ?RdfTerm:or([bnode,iri,literal]),
%!   -NumberOfTriples:nonneg
%! ) is det.

rdf_triples_by_term(G, T, N):-
  rdf_triples_by_subject(  G, T, NS),
  rdf_triples_by_predicate(G, T, NP),
  rdf_triples_by_object(   G, T, NO),
  rdf_triples_by_datatype( G, T, ND),
  sum_list([NS,NP,NO,ND], N).

