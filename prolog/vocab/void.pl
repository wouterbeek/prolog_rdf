:- module(
  void,
  [
    source_to_void/4  % +DataG, +Meta, :Goal_3, +VoidG
  ]
).

/** <module> VoID

Automatically generate VoID descriptions.

@author Wouter Beek
@tbd Add support for generating linksets.
@version 2016/01-2016/02, 2016/04, 2016/06
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdfio)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(rdfs/rdfs_stat)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(true)).
:- use_module(library(yall)).

:- meta_predicate
    source_to_void(+, +, 3, +).

:- rdf_meta
   source_to_void(r, +, :, r).





%! source_to_void(+DataG, +Meta, :Goal_3, +VoidG) is det.
%
% Common assertions to be made by `call(Goal_3, M, Dataset, VoidG)`
% are:
%
%   * `dcterms:description` A textual description of the dataset.
%
%   * `dcterms:contributor` An entity, such as a person, organisation,
%   or service, that is responsible for making contributions to the
%   dataset.  The contributor should be described as an RDF resource,
%   rather than just providing the name as a literal.
%
%   * `dcterms:created ` Date of creation of the dataset. The value
%   should be formatted and data-typed as an xsd:date.
%
%   * `dcterms:creator ` An entity, such as a person, organisation, or
%   service, that is primarily responsible for creating the dataset.
%   The creator should be described as an RDF resource, rather than
%   just providing the name as a literal.
%
%   * `dcterms:date` A point or period of time associated with an event
%   in the life-cycle of the resource.  The value should be formatted
%   and data-typed as an xsd:date.
%
%   * `dcterms:issued` Date of formal issuance (e.g., publication) of
%   the dataset.  The value should be formatted and datatyped as an
%   xsd:date.
%
%   * `dcterms:modified` Date on which the dataset was changed.  The
%   value should be formatted and datatyped as an xsd:date.
%
%   * `dcterms:publisher` An entity, such as a person, organisation, or
%   service, that is responsible for making the dataset available.
%   The publisher should be described as an RDF resource, rather than
%   just providing the name as a literal.
%
%   * `dcterms:source` A related resource from which the dataset is
%   derived.  The source should be described as an RDF resource,
%   rather than as a literal.
%
%   * `dcterms:title ` The name of the dataset.
%
%   * `foaf:homepage` _The_ (IFP) homepage of the dataset.
%
%   * `foaf:page` A Web page containing relevant information.

source_to_void(DataG, Meta, Goal_3, VoidG) :-
  M = hdt,
  
  % rdf:type
  qb_bnode(Dataset),
  qb_instance(M, Dataset, void:'Dataset', VoidG),
  
  % @tbd
  %% Number of unique classes (‘void:classes’).
  %rdfs_number_of_classes(DataG, NumCs),
  %qb(M, Dataset, void:classes, NumCs^^xsd:nonNegativeInteger, VoidG),
  
  % Link where a data dump is published (‘void:dataDump’).
  qb(M, Dataset, void:dataDump, Meta.base_iri, VoidG),

  % Number of distinct object terms (‘void:distinctObjects’).
  q_number_of_objects(M, DataG, NumOs),
  qb(M, Dataset, void:distinctObjects, NumOs^^xsd:nonNegativeInteger, VoidG),

  % Number of distinct subject terms (‘void:distinctSubjects’).
  q_number_of_subjects(M, DataG, NumSs),
  qb(M, Dataset, void:distinctSubjects, NumSs^^xsd:nonNegativeInteger, VoidG),
  
  %% void:feature
  jsonld_metadata_expand_iri(Meta.rdf_format, Format),
  qb(M, Dataset, void:feature, Format, VoidG),
  
  % Number of distinct predicate terms (erroneously called
  % ‘void:properties’).
  q_number_of_predicates(M, DataG, NumPs),
  qb(M, Dataset, void:properties, NumPs^^xsd:nonNegativeInteger, VoidG),
  
  % Number of distinct triples (‘void:triples’).  Should we not count
  % quadruples?
  q_number_of_triples(M, DataG, NumTriples),
  qb(M, Dataset, void:triples, NumTriples^^xsd:nonNegativeInteger, VoidG),

  % Vocabularies that appear in the data, defined as the registered
  % RDF prefixes that appear in at least one of the RDF terms
  % (‘void:vocabulary’).
  forall(
    distinct(Vocab, (
      vocab_term(Iri, DataG),
      q_is_iri(Iri),
      q_iri_prefix(Iri, Vocab)
    )),
    qb(M, Dataset, void:vocabulary, Vocab, VoidG)
  ),

  % VoID assertions that cannot be generated automatically.
  call(Goal_3, M, Dataset, VoidG).
  % @tbd
  %(current_goal(Goal_3) -> call(Goal_3, M, Dataset, VoidG) ; true).


vocab_term(C, G) :-
  rdfs_class(C, G).
vocab_term(P, G) :-
  rdfs_property(P, G).


current_goal(Goal0) :-
  strip_module(Goal0, _, Goal),
  Goal =.. [Pred|Args],
  length(Args, Arity),
  current_predicate(Pred/Arity).
