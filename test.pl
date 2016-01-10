:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_print)).

test:-
  rdf_assert(ex:a, rdfs:label, "o"),
  rdf_assert(ex:a, owl:sameAs, ex:b),
  rdf_print_graph(default, [id_closure(true)]).
:- test.
