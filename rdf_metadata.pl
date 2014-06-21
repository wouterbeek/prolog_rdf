:- module(
  rdf_metadata,
  [
  ]
).

/** <module> RDF metadata

Tools for extracting metadata statements from RDF data.

The following metadata vocabularies are supported:
  * SPARQL 1.1 Service Description

@author Wouter Beek
@see http://www.w3.org/ns/sparql-service-description#
@version 2014/06
*/

:- use_module(library(persistency)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(db_ext)).
:- use_module(os(file_ext)).

:- use_module(plRdf_ser(rdf_serial)).

:- db_add_novel(user:prolog_file_type(log, logging)).

%! meta_class(?Vocabulary:atom, ?Class:atom) is nondet.

:- persistent(meta_class(vocabulary:atom,class:atom)).

%! meta_property(?Vocabulary:atom, ?Class:atom) is nondet.

:- persistent(meta_property(vocabulary:atom,class:atom)).

:- initialization(rdf_metadata_init).



%! meta_namespace(?Vocabulary:atom, ?IriPrefix:atom) is nondet.

meta_namespace(sd, 'http://www.w3.org/ns/sparql-service-description#').
meta_namespace(void, 'http://rdfs.org/ns/void#').



% Initialization

%! rdf_metadata_assert(+Graph:atom, +Vocabulary:atom) is det.

rdf_metadata_assert(Graph, Vocabulary):-
  rdf_metadata_assert_classes(Graph, Vocabulary),
  rdf_metadata_assert_properties(Graph, Vocabulary).


%! rdf_metadata_assert_classes(+Graph:atom, +Vocabulary:atom) is det.

rdf_metadata_assert_classes(Graph, Vocabulary):-
  forall(
    (
      rdf(_, _, Class, Graph),
      rdf_global_id(Vocabulary:LocalName, Class)
    ),
    assert_meta_class(Vocabulary, LocalName)
  ).


%! rdf_metadata_assert_properties(+Graph:atom, +Vocabulary:atom) is det.

rdf_metadata_assert_properties(Graph, Vocabulary):-
  forall(
    (
      rdf(_, Property, _, Graph),
      rdf_global_id(Vocabulary:LocalName, Property)
    ),
    assert_meta_property(Vocabulary, LocalName)
  ).


%! rdf_metadata_download is det.

rdf_metadata_download:-
  forall(
    meta_namespace(Vocabulary, Url),
    rdf_metadata_download(Vocabulary, Url)
  ).


%! rdf_metadata_download(+Vocabulary:atom, +Url:url) is det.

rdf_metadata_download(Vocabulary, Url):-
  setup_call_cleanup(
    rdf_load_any([graph(Graph)], Url),
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
  safe_db_attach(File),
  file_age(File, Age),
  rdf_metadata_update(Age).


%! rdf_metadata_update(+Age:float) is det.

% The persistent store is still fresh.
rdf_metadata_update(Age):-
  once((meta_class(_, _) ; meta_property(_, _))),
  Age < 8640000, !.
% The persistent store has become stale, so refresh it.
rdf_metadata_update(_):-
  retractall_meta_class(_, _),
  retractall_meta_property(_, _),
  rdf_metadata_download.


%! safe_db_attach(+File:atom) is det.

safe_db_attach(File):-
  exists_file(File), !,
  db_attach(File, []).
safe_db_attach(File):-
  touch_file(File),
  safe_db_attach(File).

