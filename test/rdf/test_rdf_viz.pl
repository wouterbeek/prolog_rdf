:- module(test_rdf_viz).

:- use_module(library(gv/gv_file)).
:- use_module(library(mat/mat)).
:- use_module(library(mat/mat_viz)).
:- use_module(library(q/q_graph_viz)).
:- use_module(library(q/qb)).
:- use_module(library(os/process_ext)).
:- use_module(library(rdf/rdf_show)).
:- use_module(library(semweb/rdf11)).



script1:-
  rdf_reset_db,
  M = trp,
  G = script1,
  qb_bnode(D),
  qb(M, ex:'A', owl:equivalentClass, D, G),
  owl_assert_value_restriction(M, ex:p, ex:v1, G, R1),
  owl_assert_value_restriction(M, ex:p, ex:v2, G, R2),
  qb_list(M, [R1,R2], Rs, G),
  qb(M, D, owl:intersectionOf, Rs, G),
  qb_instance(M, ex:a, ex:'A', G),
  q_graph_viz(G),
  mat(G, GMat),
  q_graph_viz(GMat).

script2:-
  rdf_reset_db,
  M = trp,
  G = script2,
  qb_bnode(D),
  qb(M, ex:'A', owl:equivalentClass, D, G),
  owl_assert_value_restriction(ex:p, ex:v1, G, R1),
  owl_assert_value_restriction(ex:p, ex:v2, G, R2),
  qb_list(M, [R1,R2], Rs, G),
  qb(M, D, owl:intersectionOf, Rs, G),
  qb_instance(M, ex:a, ex:'A', G),
  mat(G),
  mat_viz.

script3:-
  rdf_reset_db,
  M = trp,
  G = script3,
  qb(M, ex:a, owl:sameAs, ex:b, G),
  qb(M, ex:b, owl:differentFrom, ex:a, G),
  mat(G),
  mat_viz.

script4:-
  rdf_reset_db,
  M = trp,
  G = script4,
  qb_range(M, ex:p, ex:c, G),
  qb(M, ex:s, ex:p, "o", G),
  mat(G),
  mat_viz.

script5:-
  rdf_reset_db,
  M = trp,
  G = script5,
  owl_assert_functional_property(ex:p, G),
  qb(M, ex:a, ex:p, ex:b, G),
  qb(M, ex:a, ex:p, ex:c, G),
  qb(M, ex:b, owl:differentFrom, ex:c, G),
  mat(G),
  mat_viz.
