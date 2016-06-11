:- module(
  void,
  [
    source_to_void/2 % +Source, +G
  ]
).

/** <module> VoID

Automatically generate VoID descriptions.

@author Wouter Beek
@version 2016/01-2016/02, 2016/04, 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(stat/rdf_stat)).
:- use_module(library(stat/rdfs_stat)).
:- use_module(library(yall)).

:- rdf_meta
   source_to_void(+, r).





%! source_to_void(+Source, ?VoidG) is det.

source_to_void(Source, G2) :-
  rdf_call_on_graph(Source, {G2}/[G1,M,M]>>graph_to_void0(G1, M, G2)).


graph_to_void0(G1, M, G2) :-
  % rdf:type
  rdf_create_bnode(Dataset),
  rdf_assert_instance(Dataset, void:'Dataset', G2),
  
  % Number of unique classes (‘void:classes’).
  rdfs_number_of_classes(G1, NumCs),
  rdf_assert(Dataset, void:classes, NumCs^^xsd:nonNegativeInteger, G2),
  
  % Link where a data dump is published (‘void:dataDump’).
  rdf_assert(Dataset, void:dataDump, M.'llo:base_iri', G2),

  % Number of distinct object terms (‘void:distinctObjects’).
  rdf_number_of_objects(G1, NumOs),
  rdf_assert(Dataset, void:distinctObjects, NumOs^^xsd:nonNegativeInteger, G2),

  % Number of distinct subject terms (‘void:distinctSubjects’).
  rdf_number_of_subjects(G1, NumSs),
  rdf_assert(Dataset, void:distinctSubjects, NumSs^^xsd:nonNegativeInteger, G2),
  
  % void:feature
  jsonld_metadata_expand_iri(M.'llo:rdf_format', Format),
  rdf_assert(Dataset, void:feature, Format, G2),
  
  % Number of distinct predicate terms (erroneously called
  % ‘void:properties’).
  rdf_number_of_predicates(G1, NumPs),
  rdf_assert(Dataset, void:properties, NumPs^^xsd:nonNegativeInteger, G2),
  
  % Number of distinct triples (‘void:triples’).  Should we not count
  % quadruples?
  rdf_number_of_triples(G1, NumTriples),
  rdf_assert(Dataset, void:triples, NumTriples^^xsd:nonNegativeInteger, G2),
  
  % Vocabularies that appear in the data, defined as the registered
  % RDF prefixes that appear in at least one of the RDF terms
  % (‘void:vocabulary’).
  forall(
    distinct(Vocab, (
      vocab_term(Iri, G1),
      rdf_is_iri(Iri),
      rdf_iri_prefix(Iri, Vocab)
    )),
    rdf_assert(Dataset, void:vocabulary, Vocab, G2)
  ).


vocab_term(C, G) :-
  rdfs_class(C, G).
vocab_term(P, G) :-
  rdfs_property(P, G).
