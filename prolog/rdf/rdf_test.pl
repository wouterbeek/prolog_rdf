:- module(
  rdf_test,
  [
    rdf_test/2 % +Name:compound, -G
  ]
).

/** <module> RDF tests

Various tests for RDF predicates,

@author Wouter Beek
@version 2015/12-2016/01
*/

:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_print)).

:- rdf_register_prefix(ch, 'http://www.wouterbeek.com/ch.owl#' ).
:- rdf_register_prefix(nl, 'http://www.wouterbeek.com/nl.owl#' ).

:- meta_predicate(rdf_test(0,0)).




rdf_test(database(identity), G) :-
  rdf_equal(ex:identity, G),
  rdf_test((
    rdfs_assert_label(ex:a, "a", G),
    rdfs_assert_label(ex:b, "b", G),
    rdf_assert(ex:a, owl:sameAs, ex:b, G),
    rdf_assert(ex:a, owl:sameAs, ex:c, G),
    rdf_assert(ex:d, owl:sameAs, ex:c, G)
  ),(
    Opts = [indent(2)],
    format("Graph:~n"),
    rdf_print_graph(default, [id_closure(true)|Opts]),
    format("Identity store:~n"),
    rdf_print_graph(default, Opts)
  )).



rdf_test(modeling(visum), G) :-
  % Use a default graph name if none is given.
  rdf_equal(ex:visum, G),

  % Chinese namespace
  rdfs_assert_subclass(ch:capital, ch:cityWithAirport, G),
  rdf_assert_instance(ch:'Amsterdam', ch:capital, G),
  rdfs_assert_subclass(ch:europeanCity, ch:visumNeeded, G),
  rdf_assert_instance(ch:'Amsterdam', ch:europeanCity, G),

  % Dutch namespace
  rdfs_assert_subclass(nl:visumFree, nl:europeanCity, G),
  rdf_assert_instance(nl:'Amsterdam', nl:europeanCity, G),
  rdf_assert_instance(nl:'Amsterdam', nl:capital, G),

  % Interrelations
  owl_assert_equivalent_class(ch:capital, nl:capital, G),
  rdf_assert(dbr:'Amsterdam', owl:sameAs, ch:'Amsterdam', G),
  rdf_assert(dbr:'Amsterdam', owl:sameAs, nl:'Amsterdam', G).


/*
rdf_test(semantics(graph_instance), _) :-
  rdf_test((
    maplist(rdf_unload_graph, [ex:'test-graph',ex:'test-graph-instance']),
    maplist(rdf_create_bnode, [X1,X2,X3,X4]),
    rdf_assert(X1, rdf:p, X2, ex:'test-graph'),
    rdf_assert(X3, rdf:p, X4, ex:'test-graph'),
    rdf_assert(rdf:a, rdf:p, rdf:b, ex:'test-graph-instance'),
    rdf_assert(rdf:c, rdf:p, rdf:d, ex:'test-graph-instance')
  ),(
    forall(
      rdf_graph_instance(ex:'test-graph-instance', ex:'test-graph', Map),
      dcg_with_output_to(user_output, (list(term, Map), nl))
    )
  )).
*/

rdf_test(Setup_0, Test_0) :-
  rdf_transaction((Setup_0, Test_0)).
