:- module(
  rdf_metadata,
  [
    rdf_metadata/3, % ?Subject:or([bnode,iri])
                    % ?Predicate:iri
                    % ?Object:or([bnode,iri,literal])
    rdf_metadata/4 % ?Subject:or([bnode,iri])
                   % ?Predicate:iri
                   % ?Object:or([bnode,iri,literal])
                   % ?Graph:atom
  ]
).

/** <module> RDF metadata

Tools for extracting metadata statements from RDF data.

The following metadata vocabularies are supported:
  * SPARQL 1.1 Service Description

@author Wouter Beek
@see http://www.w3.org/ns/sparql-service-description#
@version 2014/06, 2014/08
*/

:- use_module(library(persistency)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(semweb/rdf_http_plugin)). % HTTP support for RDF load.

:- use_module(generics(db_ext)).
:- use_module(generics(persistent_db_ext)).
:- use_module(os(file_ext)).

:- db_add_novel(user:prolog_file_type(log, logging)).

%! rdf_meta_class(?Vocabulary:atom, ?Class:atom) is nondet.

:- persistent(rdf_meta_class(vocabulary:atom,class:atom)).

%! rdf_meta_property(?Vocabulary:atom, ?Class:atom) is nondet.

:- persistent(rdf_meta_property(vocabulary:atom,class:atom)).

:- rdf_meta(rdf_metadata(r,r,o)).
:- rdf_meta(rdf_metadata(r,r,o,?)).

:- initialization(rdf_metadata_init).



%! rdf_contains_metadata(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:or([bnode,iri,literal])
%! ) is semidet.

rdf_contains_metadata(_, P, _):-
  rdf_meta_property(_, P), !.
rdf_contains_metadata(S, _, _):-
  rdf_meta_class(_, S), !.
rdf_contains_metadata(_, _, O):-
  rdf_meta_class(_, O), !.


%! rdf_meta_namespace(?Vocabulary:atom, ?IriPrefix:atom) is nondet.

rdf_meta_namespace(dc, 'http://purl.org/dc/elements/1.1/').
rdf_meta_namespace(dcterms, 'http://purl.org/dc/terms/').
rdf_meta_namespace(dctype, 'http://purl.org/dc/dcmitype/').
rdf_meta_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
rdf_meta_namespace(sd, 'http://www.w3.org/ns/sparql-service-description#').
rdf_meta_namespace(vann, 'http://purl.org/vocab/vann/').
rdf_meta_namespace(void, 'http://rdfs.org/ns/void#').
rdf_meta_namespace(wv, 'http://vocab.org/waiver/terms/norms').


%! rdf_metadata(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal])
%! ) is nondet.

rdf_metadata(S, P, O):-
  rdf_metadata(S, P, O, _).

%! rdf_metadata(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   ?Graph:atom
%! ) is nondet.

rdf_metadata(S, P, O, G):-
  rdf(S, P, O, G),
  rdf_contains_metadata(S, P, O).



% Initialization

%! rdf_metadata_assert(+Graph:atom, +Vocabulary:atom) is det.

rdf_metadata_assert(Graph, Vocabulary):-
  rdf_metadata_assert_classes(Graph, Vocabulary),
  rdf_metadata_assert_properties(Graph, Vocabulary).


%! rdf_metadata_assert_classes(+Graph:atom, +Vocabulary:atom) is det.

rdf_metadata_assert_classes(Graph, Vocabulary):-
  forall(
    (
      rdf(Class, rdf:type, rdfs:'Class', Graph),
      rdf_global_id(Vocabulary:LocalName, Class)
    ),
    assert_rdf_meta_class0(Vocabulary, LocalName)
  ).
assert_rdf_meta_class0(Vocabulary, LocalName):-
  rdf_meta_class(Vocabulary, LocalName), !.
assert_rdf_meta_class0(Vocabulary, LocalName):-
  assert_rdf_meta_class(Vocabulary, LocalName).


%! rdf_metadata_assert_properties(+Graph:atom, +Vocabulary:atom) is det.

rdf_metadata_assert_properties(Graph, Vocabulary):-
  forall(
    (
      rdf(Property, rdf:type, rdf:'Property', Graph),
      rdf_global_id(Vocabulary:LocalName, Property)
    ),
    assert_rdf_meta_property0(Vocabulary, LocalName)
  ).
assert_rdf_meta_property0(Vocabulary, LocalName):-
  rdf_meta_property(Vocabulary, LocalName), !.
assert_rdf_meta_property0(Vocabulary, LocalName):-
  assert_rdf_meta_property(Vocabulary, LocalName).


%! rdf_metadata_download is det.

rdf_metadata_download:-
  forall(
    rdf_meta_namespace(Vocabulary, Url),
    rdf_metadata_download(Vocabulary, Url)
  ).


%! rdf_metadata_download(+Vocabulary:atom, +Url:url) is det.

rdf_metadata_download(Vocabulary, Url):-
  rdf_register_prefix(Vocabulary, Url),
  Graph = Url,
  setup_call_cleanup(
    rdf_load(Url, [graph(Graph)]),
    rdf_metadata_assert(Graph, Vocabulary),
    rdf_unload_graph(Graph)
  ).


%! rdf_metadata_file(-File:atom) is det.

rdf_metadata_file(File):-
  absolute_file_name(
    data(rdf_metadata),
    File,
    [access(write),file_type(logging)]
  ).


%! rdf_metadata_init is det.

rdf_metadata_init:-
  rdf_metadata_file(File),
  persistent_db_init(File, rdf_metadata_update).


%! rdf_metadata_update(+Age:float) is det.

% The persistent store is still fresh.
rdf_metadata_update(Age):-
  once((rdf_meta_class(_, _) ; rdf_meta_property(_, _))),
  Age < 8640000, !. % 10 days.
% The persistent store has become stale, so refresh it.
rdf_metadata_update(_):-
  retractall_rdf_meta_class(_, _),
  retractall_rdf_meta_property(_, _),
  rdf_metadata_download.

