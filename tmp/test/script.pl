:- use_module(library(debug)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(uri/uri_ext)).

script1(M) :-
  rdf_reset_db,
  rdf_equal(ex:script1, G),
  fresh_uri(Hog1, uri(_,_,[animal,hog],_,_)),
  fresh_uri(Hog2, uri(_,_,[animal,hog],_,_)),
  rdf_assert_instance(M, Hog1, ex:'Hog', G),
  rdf_assert_instance(M, Hog2, ex:'Hog', G),
  rdf_assert(M, Hog1, foaf:knows, Hog2, G),
  rdf_assert(M, Hog1, ex:age, 2^^xsd:nonNegativeInteger, G),
  rdf_assert_now(M, Hog1, ex:registrationDate, G),
  rdf_assert(M, Hog2, ex:age, 2.3, G),
  rdf_assert(M, Hog2, ex:age, 23 rdiv 10, G),
  rdf_assert(M, Hog2, rdfs:comment, "This is a fine hog.", G),
  rdf_print_graph(G).

script2(M) :-
  rdf_reset_db,
  rdf_equal(ex:script2, G),
  rdf_create_well_known_iri(D),
  rdf_assert(M, ex:'A', owl:equivalentClass, D, G),
  rdf_assert_value_restriction(ex:p, ex:v1, G, R1),
  rdf_assert_value_restriction(ex:p, ex:v2, G, R2),
  rdf_assert_intersection_of(D, [R1,R2], G),
  rdf_assert_instance(M, ex:a, ex:'A', G),
  rdf_print_graph(G).

script3(M) :-
  rdf_reset_db, 
  rdf_equal(ex:script3, G),
  rdf_assert(M, ex:a, owl:sameAs, ex:b, G),
  rdf_assert(M, ex:b, owl:differentFrom, ex:a, G),
  rdf_print_graph(G).

script4(M) :-
  rdf_reset_db,
  rdf_equal(ex:script4, G),
  rdf_assert_range(M, ex:p, ex:c, G),
  rdf_assert(M, ex:s, ex:p, "o", G),
  rdf_print_graph(G).

script5(M) :-
  rdf_reset_db,
  rdf_equal(ex:script5, G),
  rdf_assert_functional_property(ex:p, G),
  rdf_assert(M, ex:a, ex:p, ex:b, G),
  rdf_assert(M, ex:a, ex:p, ex:c, G),
  rdf_assert(M, ex:b, owl:differentFrom, ex:c, G),
  rdf_print_graph(G).

script6(M) :-
  rdf_reset_db,
  rdf_equal(ex:script6, G),
  rdf_assert_now(M, ex:s, ex:p, G),
  rdf_print_graph(G).


script7(M) :-
  rdf_assert(M, ex:s, ex:p, 01^^xsd:integer, ex:g),
  print_id_store,
  rdf_print_quads(M, _, _, _, _),
  rdf_assert(M, ex:s, owl:sameAs, 1^^xsd:integer, ex:g),
  print_id_store,
  rdf_print_quads(M, _, _, _, _).
