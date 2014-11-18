:- module(
  rdf_script,
  [
    assert_visum/1 % ?Graph:atom
  ]
).

/** <module> RDF script

Scripts for asserting RDF graphs that can be used for debugging.

[[rdfs.png]]

@author Wouter Beek
@version 2012/12-2013/02, 2013/07, 2014/06
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(owl/owl_build)).

:- rdf_register_prefix(ch, 'http://www.wouterbeek.com/ch.owl#' ).
:- rdf_register_prefix(dbpedia, 'http://dbpedia.org/resource/').
:- rdf_register_prefix(nl, 'http://www.wouterbeek.com/nl.owl#' ).



assert_visum(G):-
  % Use a default graph name if none is given.
  (
    var(G)
  ->
    G = visum
  ;
    true
  ),

  % Chinese namespace
  rdfs_assert_class(    ch:cityWithAirport,                     G),
  rdfs_assert_subclass( ch:capital,         ch:cityWithAirport, G),
  rdf_assert_instance(ch:'Amsterdam',     ch:capital,         G),
  rdfs_assert_class(    ch:visumNeeded,                         G),
  rdfs_assert_subclass( ch:europeanCity,    ch:visumNeeded,     G),
  rdf_assert_instance(ch:'Amsterdam',     ch:europeanCity,    G),

  % Dutch namespace
  rdfs_assert_class(    nl:europeanCity,                   G),
  rdfs_assert_subclass( nl:visumFree,    nl:europeanCity,  G),
  rdf_assert_instance(nl:'Amsterdam',  nl:europeanCity,  G),
  rdfs_assert_class(    nl:capital,                        G),
  rdf_assert_instance(nl:'Amsterdam',  nl:capital,       G),

  % Interrelations
  owl_assert_class_equivalence(ch:capital,      nl:capital,     G),
  owl_assert_resource_identity(dbpedia:'Amsterdam', ch:'Amsterdam', G),
  owl_assert_resource_identity(dbpedia:'Amsterdam', nl:'Amsterdam', G).

