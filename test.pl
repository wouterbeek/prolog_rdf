:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_read)).

test:-
  rdf_assert_literal(ex:a, rdfs:label, xsd:string, o),
  rdf_assert(ex:a, owl:sameAs, ex:b),
  rdf_print_graph(default, [id_closure(true)]).
:- test.
