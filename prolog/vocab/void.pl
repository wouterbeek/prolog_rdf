:- module(
  void,
  [
    qb_license/4,    % +M, +S, +Name, +G
    qb_norms/4,      % +M, +S, +Name, +G
    qb_void_graph/6, % +M, +D, +GName, +G, :Goal_4, +MetaG
    qb_waiver/4      % +M, +S, +Lit, +G
  ]
).

/** <module> VoID

Automatically generate VoID descriptions.

@author Wouter Beek
@tbd Add support for generating linksets.
@version 2016/01-2016/02, 2016/04, 2016/06, 2016/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(jsonld/jsonld_metadata)).
:- use_module(library(licenses)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(q/qb)).
:- use_module(library(rdf/rdf__io)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(solution_sequences)).
:- use_module(library(true)).
:- use_module(library(yall)).

:- meta_predicate
    qb_void_graph(+, +, +, +, 4, +).

:- rdf_meta
   qb_license(+, r, +, r),
   qb_norms(+, r, +, r),
   qb_void_graph(+, r, +, r, :, r),
   qb_waiver(+, r, o, r).





%! qb_license(+M, +S, +Name, +G) is det.

qb_license(M, S, Name, G) :-
  license(Name, Url),
  qb(M, S, dct:license, Url, G).



%! qb_norms(+M, +S, +Name, +G) is det.

qb_norms(M, S, Name, G) :-
  norms(Name, Url),
  qb(M, S, wv:norms, Url, G).



%! qb_void_graph(+M, +D, +GName, +G, :Goal_4, +MetaG) is det.
%
% Common assertions to be made by `call(Goal_3, M, Dataset, MetaG)`
% are:
%
%   * `dcterms:description`
%
%     A textual description of the dataset.
%
%   * `dcterms:contributor`
%
%     An entity, such as a person, organisation, or service, that is
%     responsible for making contributions to the dataset.  The
%     contributor should be described as an RDF resource, rather than
%     just providing the name as a literal.
%
%   * `dcterms:created `
%
%     Date of creation of the dataset. The value should be formatted
%     and data-typed as an xsd:date.
%
%   * `dcterms:creator `
%
%     An entity, such as a person, organisation, or service, that is
%     primarily responsible for creating the dataset.  The creator
%     should be described as an RDF resource, rather than just
%     providing the name as a literal.
%
%   * `dcterms:date`
%
%     A point or period of time associated with an event in the
%     life-cycle of the resource.  The value should be formatted and
%     data-typed as an xsd:date.
%
%   * `dcterms:issued`
%
%     Date of formal issuance (e.g., publication) of the dataset.  The
%     value should be formatted and datatyped as an xsd:date.
%
%   * `dcterms:modified`
%
%     Date on which the dataset was changed.  The value should be
%     formatted and datatyped as an xsd:date.
%
%   * `dcterms:publisher`
%
%     An entity, such as a person, organisation, or service, that is
%     responsible for making the dataset available.  The publisher
%     should be described as an RDF resource, rather than just
%     providing the name as a literal.
%
%   * `dcterms:source`
%
%     A related resource from which the dataset is derived.  The
%     source should be described as an RDF resource, rather than as a
%     literal.
%
%   * `dcterms:title`
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

qb_void_graph(M, D, GName, G, Goal_4, MetaG) :-
  qb(M, D, void:subset, G, MetaG),

  % ‘rdfs:label’
  capitalize_atom(GName, Name),
  qb_label(M, G, Name@nl, MetaG),

  % @tbd
  %% Number of unique classes (‘void:classes’).
  %rdfs_number_of_classes(DataG, NumCs),
  %qb(M, G, void:classes, NumCs^^xsd:nonNegativeInteger, MetaG),

  % @tbd Based on datadump endpoint.
  %% Link where a data dump is published (‘void:dataDump’).
  %qb(M, G, void:dataDump, Meta.base_iri, MetaG),

  % Number of distinct object terms (‘void:distinctObjects’).
  q_number_of_objects(M, G, NumOs),
  qb(M, G, void:distinctObjects, NumOs^^xsd:nonNegativeInteger, MetaG),

  % Number of distinct subject terms (‘void:distinctSubjects’).
  q_number_of_subjects(M, G, NumSs),
  qb(M, G, void:distinctSubjects, NumSs^^xsd:nonNegativeInteger, MetaG),

  % @tbd Based on datadump endpoint.
  %% void:feature
  %jsonld_metadata_expand_iri(Meta.rdf_format, Format),
  %qb(M, G, void:feature, Format, MetaG),
  
  % Number of distinct predicate terms (erroneously called
  % ‘void:properties’).
  q_number_of_predicates(M, G, NumPs),
  qb(M, G, void:properties, NumPs^^xsd:nonNegativeInteger, MetaG),
  
  % Number of distinct triples (‘void:triples’).  Should we not count
  % quadruples?
  q_number_of_triples(M, G, NumTriples),
  qb(M, G, void:triples, NumTriples^^xsd:nonNegativeInteger, MetaG),

  % @tbd
  %% Vocabularies that appear in the data, defined as the registered
  %% RDF prefixes that appear in at least one of the RDF terms
  %% (‘void:vocabulary’).
  %forall(
  %  distinct(Vocab, (
  %    vocab_term(Iri, G),
  %    q_is_iri(Iri),
  %    q_iri_prefix(Iri, Vocab)
  %  )),
  %  qb(M, G, void:vocabulary, Vocab, MetaG)
  %),

  % VoID assertions that cannot be generated automatically.
  call(Goal_4, M, GName, G, MetaG).

%vocab_term(C, G) :-
%  rdfs_class(C, G).
%vocab_term(P, G) :-
%  rdfs_property(P, G).



%! qb_waiver(+M, +S, +Lit, +G) is det.

qb_waiver(M, S, Lit, G) :-
  qb(M, S, wv:waiver, Lit, G).
