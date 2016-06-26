:- use_module(library(rdf/rdf_ext)).
:- use_module(library(z/z_print)).

test:-
  rdf_assert(ex:a, rdfs:label, "o"),
  rdf_assert(ex:a, owl:sameAs, ex:b),
  z_print_triples(_, _, _, default).
:- test.
