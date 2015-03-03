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
@version 2012/12-2013/02, 2013/07, 2014/06, 2015/03
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(generics/meta_ext)).

:- use_module(plRdf(api/owl_build)).
:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).

:- rdf_register_prefix(ch, 'http://www.wouterbeek.com/ch.owl#' ).
:- rdf_register_prefix(dbpedia, 'http://dbpedia.org/resource/').
:- rdf_register_prefix(nl, 'http://www.wouterbeek.com/nl.owl#' ).





assert_visum(Graph):-
  % Use a default graph name if none is given.
  default(visum, Graph),

  % Chinese namespace
  rdfs_assert_class(ch:cityWithAirport, Graph),
  rdfs_assert_subclass(ch:capital, ch:cityWithAirport, Graph),
  rdf_assert_instance(ch:'Amsterdam', ch:capital, Graph),
  rdfs_assert_class(ch:visumNeeded, Graph),
  rdfs_assert_subclass(ch:europeanCity, ch:visumNeeded, Graph),
  rdf_assert_instance(ch:'Amsterdam', ch:europeanCity, Graph),

  % Dutch namespace
  rdfs_assert_class(nl:europeanCity, Graph),
  rdfs_assert_subclass( nl:visumFree, nl:europeanCity, Graph),
  rdf_assert_instance(nl:'Amsterdam', nl:europeanCity, Graph),
  rdfs_assert_class(nl:capital, Graph),
  rdf_assert_instance(nl:'Amsterdam', nl:capital, Graph),

  % Interrelations
  owl_assert_class_equivalence(ch:capital, nl:capital, Graph),
  owl_assert_resource_identity(dbpedia:'Amsterdam', ch:'Amsterdam', Graph),
  owl_assert_resource_identity(dbpedia:'Amsterdam', nl:'Amsterdam', Graph).

