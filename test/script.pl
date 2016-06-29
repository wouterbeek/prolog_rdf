:- use_module(library(debug)).
%:- use_module(library(mat/mat)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_term)).
:- use_module(library(rdf/rdf_info)).

:- debug(mat(rdf(_))).
:- debug(mat(rdfs(_))).
:- debug(mat(owl(cax(_)))).
:- debug(mat(owl(cls(_)))).
:- debug(mat(owl(scm(_)))).





script1:-
  rdf_reset_db,
  rdf_equal(ex:script1, G),
  rdf_create_iri(ex, [animal,hog], Hog1),
  rdf_create_iri(ex, [animal,hog], Hog2),
  rdf_assert_instance(Hog1, ex:'Hog', G),
  rdf_assert_instance(Hog2, ex:'Hog', G),
  rdf_assert(Hog1, foaf:knows, Hog2, G),
  rdf_assert(Hog1, ex:age, 2^^xsd:nonNegativeInteger, G),
  rdf_assert_now(Hog1, ex:registrationDate, G),
  rdf_assert(Hog2, ex:age, 2.3, G),
  rdf_assert(Hog2, ex:age, 23 rdiv 10, G),
  rdf_assert(Hog2, rdfs:comment, "This is a fine hog.", G),
  q_print_graph(G).
  %mat0(G).

script2:-
  rdf_reset_db,
  rdf_equal(ex:script2, G),
  q_create_bnode(D),
  rdf_assert(ex:'A', owl:equivalentClass, D, G),
  owl_assert_value_restriction(ex:p, ex:v1, G, R1),
  owl_assert_value_restriction(ex:p, ex:v2, G, R2),
  owl_assert_intersection_of(D, [R1,R2], G),
  rdf_assert_instance(ex:a, ex:'A', G),
  q_print_graph(G).
  %mat0(G).

script3:-
  rdf_reset_db, 
  rdf_equal(ex:script3, G),
  rdf_assert(ex:a, owl:sameAs, ex:b, G),
  rdf_assert(ex:b, owl:differentFrom, ex:a, G),
  q_print_graph(G).
  %mat0(G).

script4:-
  rdf_reset_db,
  rdf_equal(ex:script4, G),
  rdfs_assert_range(ex:p, ex:c, G),
  rdf_assert(ex:s, ex:p, "o", G),
  q_print_graph(G).
  %mat0(G).

script5:-
  rdf_reset_db,
  rdf_equal(ex:script5, G),
  owl_assert_functional_property(ex:p, G),
  rdf_assert(ex:a, ex:p, ex:b, G),
  rdf_assert(ex:a, ex:p, ex:c, G),
  rdf_assert(ex:b, owl:differentFrom, ex:c, G),
  q_print_graph(G).
  %mat0(G).
%mat0(G) :- atom_concat(G, '_mat', GMat), mat(G, GMat).

script6:-
  rdf_reset_db,
  rdf_equal(ex:script6, G),
  rdf_assert_now(ex:s, ex:p, G),
  q_print_graph(G).


script7:-
  rdf_assert(ex:s, ex:p, 01^^xsd:integer, ex:g),
  print_id_store,
  q_print_quads(_, _, _, _),

  rdf_assert(ex:s, owl:sameAs, 1^^xsd:integer, ex:g),
  print_id_store,
  q_print_quads(_, _, _, _).
