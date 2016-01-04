:- module(
  void_build,
  [
    void_gen/1 % +Input
  ]
).

/** <module> VoID Build

Automatically generate VoID descriptions.

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(maplist)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_load)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_save)).
:- use_module(library(rdf/rdf_stats)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(rdfs/rdfs_stats)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).



void_gen(In):-
  G = void,
  setup_call_cleanup(
    rdf_load_any(Input, [metadata(M)]),
    (
      [E|_] = M.entries,
      void_gen(In, E.'RDF'.'serialization-format', G),
      rdf_save_file('VoID.ttl', [format(turtle),graph(G)])
    ),
    rdf_unload_graphs
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
    set(IriPrefix),
    (
      vocabulary_term(T),
      rdf_is_iri(T),
      rdf_prefix_iri(Iri, IriPrefix)
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
  maplist([Vocab]>>rdf_asssert(Dataset, void:vocabulary, Vocab, G), Vocabs).


vocabulary_term(C):- rdfs_class(C).
vocabulary_term(P):- rdfs_property(P).
