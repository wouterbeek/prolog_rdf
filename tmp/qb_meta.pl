:- module(
  rdf_assert_meta,
  [
    meta_objects/3,          % +M,     +G, -NumOs
    meta_objects/4,          % +M, +P, +G, -NumOs
    meta_properties/3,       % +M,     +G, -NumPs
    meta_subjects/3,         % +M,     +G, -NumSs
    meta_subjects/4,         % +M, +P, +G, -NumSs
    meta_triples/3,          % +M,     +G, -NumTriples
    meta_triples/4,          % +M, +P, +G, -NumTriples
    rdf_p_range/3,           % +M, +P, -Ran
    rdf_p_range/4,           % +M, +P, -Ran,   ?G
    rdf_assert_license/4,    % +M, +S, +Name,  +G
    rdf_assert_meta_graph/6, % +M, +D, +GName, +G, :Goal_4, +MetaG
    rdf_assert_norms/4,      % +M, +S, +Name,  +G
    rdf_assert_waiver/4      % +M, +S, +Lit,   +G
  ]
).

/** <module> Quine: Graph metadata

@author Wouter Beek
@tbd Count the number of blank nodes.
@tbd Count the number of IRIs.
@tbd Count the number of literals.
@tbd Add support for generating linksets.
@version 2016/01-2016/02, 2016/04, 2016/06, 2016/10-2016/11
*/

:- use_module(library(atom_ext)).
:- use_module(library(licenses)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_stat)).
:- use_module(library(rdfs/rdfs_api)).

:- meta_predicate
    rdf_assert_meta_graph(+, +, +, +, 4, +).

:- rdf_meta
   meta_objects(+, r, -),
   meta_objects(+, r, r, -),
   meta_properties(+, r, -),
   meta_subjects(+, r, -),
   meta_subjects(+, r, r, -),
   meta_triples(+, r, -),
   meta_triples(+, r, r, -),
   rdf_p_range(+, r, -),
   rdf_p_range(+, r, -, r),
   rdf_assert_license(+, r, +, r),
   rdf_assert_meta_graph(+, r, +, r, :, r),
   rdf_assert_norms(+, r, +, r),
   rdf_assert_waiver(+, r, o, r).





%! meta_objects(+M, +G, -NumOs) is det.
%! meta_objects(+M, +P, +G, -NumOs) is det.

meta_objects(M, G, NumOs) :-
  rdf_graph_meta_graph(G, MetaG),
  q(M, G, void:distinctObjects, NumOs^^xsd:nonNegativeInteger, MetaG), !.
meta_objects(M, G, NumOs) :-
  meta_objects0(M, G, NumOs).

meta_objects0(M, G, NumOs) :-
  rdf_number_of_objects(M, G, NumOs).


meta_objects(M, P, G, NumOs) :-
  rdf_graph_meta_graph(G, MetaG),
  q(M, G, void:propertyPartition, B, MetaG),
  q(M, B, void:property, P, MetaG),
  q(M, B, void:distinctObjects, NumOs^^xsd:nonNegativeInteger, MetaG), !.
meta_objects(M, P, G, NumOs) :-
  meta_objects0(M, P, G, NumOs).

meta_objects0(M, P, G, NumOs) :-
  rdf_number_of_objects(M, _, P, G, NumOs).



%! meta_properties(+M, +G, -NumPs) is det.

meta_properties(M, G, NumPs) :-
  rdf_graph_meta_graph(G, MetaG),
  q(M, G, void:properties, NumPs^^xsd:nonNegativeInteger, MetaG), !.
meta_properties(M, G, NumPs) :-
  meta_properties0(M, G, NumPs).

meta_properties0(M, G, NumPs) :-
  rdf_number_of_predicates(M, G, NumPs).



%! meta_subjects(+M, +G, -NumSs) is det.
%! meta_subjects(+M, +G, +PPartG, -NumSs) is det.

meta_subjects(M, G, NumSs) :-
  rdf_graph_meta_graph(G, MetaG),
  q(M, G, void:distinctSubjects, NumSs^^xsd:nonNegativeInteger, MetaG), !.
meta_subjects(M, G, NumSs) :-
  meta_subjects0(M, G, NumSs).

meta_subjects0(M, G, NumSs) :-
  rdf_number_of_subjects(M, G, NumSs).


meta_subjects(M, P, G, NumSs) :-
  rdf_graph_meta_graph(G, MetaG),
  q(M, G, void:propertyPartition, PPartG, MetaG),
  q(M, PPartG, void:property, P, MetaG),
  q(M, PPartG, void:distinctSubjects, NumSs^^xsd:nonNegativeInteger, MetaG), !.
meta_subjects(M, P, G, NumSs) :-
  meta_subjects0(M, P, G, NumSs).

meta_subjects0(M, P, G, NumSs) :-
  rdf_number_of_subjects(M, P, _, G, NumSs).



%! meta_triples(+M, +G, -NumTriples) is det.
%! meta_triples(+M, +G, +PPartG, -NumTriples) is det.

meta_triples(M, G, NumTriples) :-
  rdf_graph_meta_graph(G, MetaG),
  q(M, G, void:triples, NumTriples^^xsd:nonNegativeInteger, MetaG), !.
meta_triples(M, G, NumTriples) :-
  meta_triples0(M, G, NumTriples).

meta_triples0(M, G, NumTriples) :-
  rdf_number_of_triples(M, G, NumTriples).


meta_triples(M, P, G, NumTriples) :-
  rdf_graph_meta_graph(G, MetaG),
  q(M, G, void:propertyPartition, PPartG, MetaG),
  q(M, PPartG, void:property, P, MetaG),
  q(M, PPartG, void:triples, NumTriples^^xsd:nonNegativeInteger, MetaG), !.
meta_triples(M, P, G, NumTriples) :-
  meta_triples0(M, P, G, NumTriples).

meta_triples0(M, P, G, NumTriples) :-
  rdf_number_of_triples(M, _, P, _, G, NumTriples).



%! rdf_p_range(+M, +P, -Ran) is det.
%! rdf_p_range(+M, +P, -Ran, +G) is det.

rdf_p_range(M, P, Ran) :-
  rdf_p_range(M, P, Ran, _).


rdf_p_range(M, P, _, G) :-
  flag(rdf_p_range, _, 0),
  once(q(M, _, P, O0, G)),
  rdf_term_cat0(M, O0, Ran0, G),
  nb_setval(range, Ran0),
  q(M, _, P, O, G),
  flag(rdf_p_range, N, N+1), format("~D ", [N]), rdf_print_object(O), nl,
  rdf_term_cat0(M, O, Ran2, G),
  nb_getval(range, Ran1),
  rdf_merge_cat0(M, Ran1, Ran2, Ran3, G),
  nb_setval(range, Ran3),
  fail.
rdf_p_range(_, _, Ran, _) :-
  nb_getval(range, Ran),
  nb_delete(range).

rdf_term_cat0(_, Lit, D, _) :-
  rdf_is_literal(Lit), !,
  rdf_literal_datatype(Lit, D).
rdf_term_cat0(M, I, C, G) :-
  rdf_instance(M, I, C),
  \+ (
    rdf_instance(M, I, C0),
    rdf_strict_subclass(M, C0, C, G)
  ), !.
rdf_term_cat0(_, _, C, _) :-
  rdf_equal(C, rdfs:'Resource').

rdf_merge_cat0(_, D1, D2, D3, _) :-
  rdf_subdatatype_of(D1, D3),
  rdf_subdatatype_of(D2, D3), !.
rdf_merge_cat0(M, C1, C2, C3, G) :-
  rdf_subclass(M, C1, C3, G),
  rdf_subclass(M, C2, C3, G), !.



%! rdf_assert_license(+M, +S, +Name, +G) is det.

rdf_assert_license(M, S, Name, G) :-
  license(Name, Url),
  rdf_assert(M, S, dct:license, Url, G).



%! rdf_assert_meta_graph(+M, +D, +GName, +G, :Goal_4, +MetaG) is det.
%
% Common assertions to be made by `call(Goal_3, M, Dataset, MetaG)`
% are:
%
%   * `dct:description`
%
%     A textual description of the dataset.
%
%   * `dct:contributor`
%
%     An entity, such as a person, organisation, or service, that is
%     responsible for making contributions to the dataset.  The
%     contributor should be described as an RDF resource, rather than
%     just providing the name as a literal.
%
%   * `dct:created `
%
%     Date of creation of the dataset. The value should be formatted
%     and data-typed as an xsd:date.
%
%   * `dct:creator `
%
%     An entity, such as a person, organisation, or service, that is
%     primarily responsible for creating the dataset.  The creator
%     should be described as an RDF resource, rather than just
%     providing the name as a literal.
%
%   * `dct:date`
%
%     A point or period of time associated with an event in the
%     life-cycle of the resource.  The value should be formatted and
%     data-typed as an xsd:date.
%
%   * `dct:issued`
%
%     Date of formal issuance (e.g., publication) of the dataset.  The
%     value should be formatted and datatyped as an xsd:date.
%
%   * `dct:modified`
%
%     Date on which the dataset was changed.  The value should be
%     formatted and datatyped as an xsd:date.
%
%   * `dct:publisher`
%
%     An entity, such as a person, organisation, or service, that is
%     responsible for making the dataset available.  The publisher
%     should be described as an RDF resource, rather than just
%     providing the name as a literal.
%
%   * `dct:source`
%
%     A related resource from which the dataset is derived.  The
%     source should be described as an RDF resource, rather than as a
%     literal.
%
%   * `dct:title`
%
%     The name of the dataset.
%
%   * `foaf:homepage`
%
%     _The_ (IFP) homepage of the dataset.
%
%   * `foaf:page`
%
%     A Web page containing relevant information.

rdf_assert_meta_graph(M, D, GName, G, Goal_4, MetaG) :-
  rdf_assert(M, D, void:subset, G, MetaG),

  % ‘rdfs:label’
  capitalize_atom(GName, Name),
  rdf_assert_label(M, G, Name@nl, MetaG),

  % @tbd
  %% Number of unique classes (‘void:classes’).
  %rdfs_number_of_classes(DataG, NumCs),
  %rdf_assert(M, G, void:classes, NumCs^^xsd:nonNegativeInteger, MetaG),

  % @tbd Based on datadump endpoint.
  %% Link where a data dump is published (‘void:dataDump’).
  %rdf_assert(M, G, void:dataDump, Meta.base_iri, MetaG),

  % Number of distinct object terms (‘void:distinctObjects’).
  % Number of distinct subject terms (‘void:distinctSubjects’).
  % Number of distinct predicate terms (erroneously called
  % ‘void:properties’).
  % Number of distinct triples (‘void:triples’).
  rdf_assert_meta_objects(M, G, MetaG),
  rdf_assert_meta_subjects(M, G, MetaG),
  rdf_assert_meta_properties(M, G, MetaG),
  rdf_assert_meta_triples(M, G, MetaG),

  % @tbd Based on datadump endpoint.
  %% void:feature
  %jsonld_metadata_expand_iri(Meta.rdf_media_type, MT),
  %rdf_assert(M, G, void:feature, MT, MetaG),

  rdf_assert_meta_class_partitions(M, G, MetaG),
  rdf_assert_meta_predicate_partitions(M, G, MetaG),

  % @tbd
  %% Vocabularies that appear in the data, defined as the registered
  %% RDF prefixes that appear in at least one of the RDF terms
  %% (‘void:vocabulary’).
  %forall(
  %  distinct(Vocab, (
  %    vocab_term(Iri, G),
  %    rdf_is_iri(Iri),
  %    rdf_iri_prefix(Iri, Vocab)
  %  )),
  %  rdf_assert(M, G, void:vocabulary, Vocab, MetaG)
  %),

  % Metadata assertions that cannot be generated automatically.
  call(Goal_4, M, GName, G, MetaG).


rdf_assert_meta_class_partition(M, C, G, MetaG) :-
  rdf_create_bnode(BNode),
  rdf_assert(M, G, void:classPartition, BNode, MetaG),

  rdf_number_of_instances(M, C, G, NumIs),
  rdf_assert(M, G, void:entities, NumIs, MetaG).


rdf_assert_meta_class_partitions(M, G, MetaG) :-
  forall(
    rdf_class(M, C, G),
    rdf_assert_meta_class_partition(M, C, G, MetaG)
  ).


rdf_assert_meta_predicate_partition(M, P, G, MetaG) :-
  rdf_create_bnode(PPartG),
  rdf_assert(M, G, void:propertyPartition, PPartG, MetaG),
  rdf_assert_meta_objects(M, P, G, PPartG, MetaG),
  rdf_assert_meta_subjects(M, P, G, PPartG, MetaG),
  rdf_assert_meta_triples(M, P, G, PPartG, MetaG),
  rdf_assert(M, PPartG, void:property, P, MetaG).


rdf_assert_meta_predicate_partitions(M, G, MetaG) :-
  forall(
    rdf_predicate(M, P, G),
    rdf_assert_meta_predicate_partition(M, P, G, MetaG)
  ).



%! rdf_assert_norms(+M, +S, +Name, +G) is det.

rdf_assert_norms(M, S, Name, G) :-
  norms(Name, Url),
  rdf_assert(M, S, wv:norms, Url, G).



%! rdf_assert_meta_objects(+M, +G, +MetaG) is det.
%! rdf_assert_meta_objects(+M, +P, +G, +PPartG, +MetaG) is det.

rdf_assert_meta_objects(M, G, MetaG) :-
  meta_objects0(M, G, NumOs),
  rdf_assert(M, G, void:distinctObjects, NumOs^^xsd:nonNegativeInteger, MetaG).


rdf_assert_meta_objects(M, P, G, PPartG, MetaG) :-
  meta_objects0(M, P, G, NumOs),
  rdf_assert(M, PPartG, void:distinctObjects, NumOs^^xsd:nonNegativeInteger, MetaG).



%! rdf_assert_meta_properties(+M, +G, +MetaG) is det.

rdf_assert_meta_properties(M, G, MetaG) :-
  meta_properties0(M, G, NumPs),
  rdf_assert(M, G, void:properties, NumPs^^xsd:nonNegativeInteger, MetaG).



%! rdf_assert_meta_subjects(+M, +G, +MetaG) is det.
%! rdf_assert_meta_subjects(+M, +P, +G, +PPartG, +MetaG) is det.

rdf_assert_meta_subjects(M, G, MetaG) :-
  meta_subjects0(M, G, NumSs),
  rdf_assert(M, G, void:distinctSubjects, NumSs^^xsd:nonNegativeInteger, MetaG).


rdf_assert_meta_subjects(M, P, G, PPartG, MetaG) :-
  meta_subjects0(M, P, G, NumSs),
  rdf_assert(M, PPartG, void:distinctSubjects, NumSs^^xsd:nonNegativeInteger, MetaG).



%! rdf_assert_meta_triples(+M, +G, +MetaG) is det.
%! rdf_assert_meta_triples(+M, +P, +G, +PPartG, +MetaG) is det.

rdf_assert_meta_triples(M, G, MetaG) :-
  meta_triples0(M, G, NumTriples),
  rdf_assert(M, G, void:triples, NumTriples^^xsd:nonNegativeInteger, MetaG).


rdf_assert_meta_triples(M, P, G, PPartG, MetaG) :-
  meta_triples0(M, P, G, NumTriples),
  rdf_assert(M, PPartG, void:triples, NumTriples^^xsd:nonNegativeInteger, MetaG).



%! rdf_assert_waiver(+M, +S, +Lit, +G) is det.

rdf_assert_waiver(M, S, Lit, G) :-
  rdf_assert(M, S, wv:waiver, Lit, G).
