:- use_module(library(debug)).
:- use_module(library(mat/mat)).
:- use_module(library(owl/owl_build)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(semweb/rdf_db)).

:- debug(mat(rdf(_))).
:- debug(mat(rdfs(_))).
:- debug(mat(owl(cax(_)))).
:- debug(mat(owl(cls(_)))).
:- debug(mat(owl(scm(_)))).

:- rdf_register_prefix(ex, 'http://example.com/').





script1:-
  rdf_unload_graphs,
  G = script1,
  fresh_iri(ex, I),
  rdf_assert_instance(I, ex:'C', G),
  rdf_assert_literal(I, ex:age, xsd:nonNegativeInteger, 2, G),
  rdf_assert_now(I, ex:registrationDate, G),
  mat(G).

script2:-
  rdf_unload_graphs,
  G = script2,
  rdf_bnode(D),
  rdf_assert(ex:'A', owl:equivalentClass, D, G),
  owl_assert_value_restriction(ex:p, ex:v1, G, R1),
  owl_assert_value_restriction(ex:p, ex:v2, G, R2),
  rdf_assert_list([R1,R2], Rs, G),
  rdf_assert(D, owl:intersectionOf, Rs, G),
  rdf_assert_instance(ex:a, ex:'A', G),
  mat(G).

script3:-
  rdf_unload_graphs,
  G = script3,
  rdf_assert(ex:a, owl:sameAs, ex:b, G),
  rdf_assert(ex:b, owl:differentFrom, ex:a, G),
  mat(G).

script4:-
  rdf_unload_graphs,
  G = script4,
  rdfs_assert_range(ex:p, ex:c, G),
  rdf_assert_literal(ex:s, ex:p, xsd:string, o, G),
  mat(G).
