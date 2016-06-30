:- use_module(library(debug)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_term)).
:- use_module(library(rdf/rdf_info)).





script1(M) :-
  rdf_reset_db,
  rdf_equal(ex:script1, G),
  q_create_iri(ex, [animal,hog], Hog1),
  q_create_iri(ex, [animal,hog], Hog2),
  qb_instance(M, Hog1, ex:'Hog', G),
  qb_instance(M, Hog2, ex:'Hog', G),
  qb(M, Hog1, foaf:knows, Hog2, G),
  qb(M, Hog1, ex:age, 2^^xsd:nonNegativeInteger, G),
  qb_now(M, Hog1, ex:registrationDate, G),
  qb(M, Hog2, ex:age, 2.3, G),
  qb(M, Hog2, ex:age, 23 rdiv 10, G),
  qb(M, Hog2, rdfs:comment, "This is a fine hog.", G),
  q_print_graph(G).

script2(M) :-
  rdf_reset_db,
  rdf_equal(ex:script2, G),
  q_create_bnode(D),
  qb(M, ex:'A', owl:equivalentClass, D, G),
  owl_assert_value_restriction(ex:p, ex:v1, G, R1),
  owl_assert_value_restriction(ex:p, ex:v2, G, R2),
  owl_assert_intersection_of(D, [R1,R2], G),
  qb_instance(M, ex:a, ex:'A', G),
  q_print_graph(G).

script3(M) :-
  rdf_reset_db, 
  rdf_equal(ex:script3, G),
  qb(M, ex:a, owl:sameAs, ex:b, G),
  qb(M, ex:b, owl:differentFrom, ex:a, G),
  q_print_graph(G).

script4(M) :-
  rdf_reset_db,
  rdf_equal(ex:script4, G),
  rdfs_assert_range(M, ex:p, ex:c, G),
  qb(M, ex:s, ex:p, "o", G),
  q_print_graph(G).

script5(M) :-
  rdf_reset_db,
  rdf_equal(ex:script5, G),
  owl_assert_functional_property(ex:p, G),
  qb(M, ex:a, ex:p, ex:b, G),
  qb(M, ex:a, ex:p, ex:c, G),
  qb(M, ex:b, owl:differentFrom, ex:c, G),
  q_print_graph(G).

script6(M) :-
  rdf_reset_db,
  rdf_equal(ex:script6, G),
  qb_now(M, ex:s, ex:p, G),
  q_print_graph(G).


script7(M) :-
  qb(M, ex:s, ex:p, 01^^xsd:integer, ex:g),
  print_id_store,
  q_print_quads(M, _, _, _, _),
  qb(M, ex:s, owl:sameAs, 1^^xsd:integer, ex:g),
  print_id_store,
  q_print_quads(M, _, _, _, _).
