:- module(test_rdf_viz).

:- use_module(library(gv/gv_file)).
:- use_module(library(mat/mat)).
:- use_module(library(mat/mat_viz)).
:- use_module(library(os/process_ext)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_graph_viz)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_show)).
:- use_module(library(semweb/rdf11)).



script1:-
  rdf_reset_db,
  G = script1,
  rdf_create_bnode(D),
  rdf_assert(ex:'A', owl:equivalentClass, D, G),
  owl_assert_value_restriction(ex:p, ex:v1, G, R1),
  owl_assert_value_restriction(ex:p, ex:v2, G, R2),
  rdf_assert_list([R1,R2], Rs, G),
  rdf_assert(D, owl:intersectionOf, Rs, G),
  rdf_assert_instance(ex:a, ex:'A', G),
  rdf_graph_viz(G),
  mat(G, GMat),
  rdf_graph_viz(GMat).

script2:-
  rdf_reset_db,
  G = script2,
  rdf_create_bnode(D),
  rdf_assert(ex:'A', owl:equivalentClass, D, G),
  owl_assert_value_restriction(ex:p, ex:v1, G, R1),
  owl_assert_value_restriction(ex:p, ex:v2, G, R2),
  rdf_assert_list([R1,R2], Rs, G),
  rdf_assert(D, owl:intersectionOf, Rs, G),
  rdf_assert_instance(ex:a, ex:'A', G),
  mat(G),
  mat_viz.

script3:-
  rdf_reset_db,
  G = script3,
  rdf_assert(ex:a, owl:sameAs, ex:b, G),
  rdf_assert(ex:b, owl:differentFrom, ex:a, G),
  mat(G),
  mat_viz.

script4:-
  rdf_reset_db,
  G = script4,
  rdfs_assert_range(ex:p, ex:c, G),
  rdf_assert(ex:s, ex:p, "o", G),
  mat(G),
  mat_viz.

script5:-
  rdf_reset_db,
  G = script5,
  owl_assert_functional_property(ex:p, G),
  rdf_assert(ex:a, ex:p, ex:b, G),
  rdf_assert(ex:a, ex:p, ex:c, G),
  rdf_assert(ex:b, owl:differentFrom, ex:c, G),
  mat(G),
  mat_viz.
