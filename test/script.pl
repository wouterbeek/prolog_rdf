:- ensure_loaded(debug).

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_view)).
:- use_module(library(semweb/rdf_db)).

:- rdf_register_prefix(mh, 'http://moonhog.net/resource/').

script:-
  fresh_iri(mh, Hog),
  rdf_assert_instance(Hog, mh:'Hog', hog_db),
  rdf_print_graph(hog_db),
  
  rdf_assert_literal(Hog, mh:age, xsd:nonNegativeInteger, 2, hog_db),
  rdf_assert_now(Hog, mh:registrationDate, hog_db),
  rdf_print_graph(hog_db).
:- script.
