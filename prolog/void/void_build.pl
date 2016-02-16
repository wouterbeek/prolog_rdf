:- module(
  void_build,
  [
    void_gen/1 % +Input
  ]
).

/** <module> VoID Build

Automatically generate VoID descriptions.

@author Wouter Beek
@version 2016/01-2016/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_save)).
:- use_module(library(rdf/rdf_stats)).
:- use_module(library(rdfs/rdfs_stats)).
:- use_module(library(rdf11/rdf11_mt)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).



void_gen(In):-
  G = void,
  setup_call_cleanup(
    rdf_load_file(In, [metadata(M)]),
    (
      [E|_] = M.entries,
      void_gen(In, E.'llo:rdf_serialization_format', G),
      rdf_save_file('VoID.ttl', [graph(G),rdf_format(turtle)])
    ),
    rdf_reset_db
  ).


void_gen(In, Format, G):-
  % classes
  rdfs_number_of_classes(NC),
  % distinctObjects
  rdf_number_of_objects(NO),
  % distinctSubjects
  rdf_number_of_subjects(NS),
  % properties
  rdfs_number_of_properties(NP),
  % triples
  rdf_number_of_triples(NT),
  % vocabularies
  aggregate_all(
    set(Prefix),
    (
      vocabulary_term(Iri),
      rdf_is_iri(Iri),
      rdf_iri_alias_prefix_local(Iri, _, Prefix, _)
    ),
    Vocabs
  ),
  
  rdf_create_bnode(Dataset),
  
  % rdf:type
  rdf_assert_instance(Dataset, void:'Dataset', G),
  % void:classes
  rdf_assert(Dataset, void:classes, NC^^xsd:nonNegativeInteger, G),
  % void:dataDump
  rdf_assert(Dataset, void:dataDump, In, G),
  % void:distinctObjects
  rdf_assert(Dataset, void:distinctObjects, NO^^xsd:nonNegativeInteger, G),
  % void:distinctSubjects
  rdf_assert(Dataset, void:distinctSubjects, NS^^xsd:nonNegativeInteger, G),
  % void:feature
  rdf_assert(Dataset, void:feature, Format, G),
  % void:properties
  rdf_assert(Dataset, void:properties, NP^^xsd:nonNegativeInteger, G),
  % void:triples
  rdf_assert(Dataset, void:triples, NT^^xsd:nonNegativeInteger, G),
  % void:vocabulary
  maplist([Vocab]>>rdf_assert(Dataset, void:vocabulary, Vocab, G), Vocabs).


vocabulary_term(C):- rdfs_class(C).
vocabulary_term(P):- rdfs_property(P).
