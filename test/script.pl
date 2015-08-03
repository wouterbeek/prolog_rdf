:- ensure_loaded(test).

:- use_module(library(debug)).
:- use_module(library(owl/owl_mat)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

:- debug(mat(rdfs(_))).
:- debug(mat(owl(cax(_)))).
:- debug(mat(owl(cls(_)))).
:- debug(mat(owl(scm(_)))).

:- rdf_register_prefix(mh, 'http://moonhog.net/resource/').

script1:-
  fresh_iri(mh, Hog),
  rdf_assert_instance(Hog, mh:'Hog', hog_db),
  rdf_print_graph(hog_db),
  
  rdf_assert_literal(Hog, mh:age, xsd:nonNegativeInteger, 2, hog_db),
  rdf_assert_now(Hog, mh:registrationDate, hog_db),
  rdf_print_graph(hog_db).

script2:-
  rdf_assert(mh:'C', owl:equivalentClass, mh:'D', hog_db),
  rdf_assert(mh:x, rdf:type, mh:'C', hog_db),
  owl_mat(hog_db).
