:- use_module(library(q/q_print)).
:- use_module(library(rdf/rdf_ext)).

test:-
  rdf_assert(ex:a, rdfs:label, "o"),
  rdf_assert(ex:a, owl:sameAs, ex:b),
  q_print_triples(_, _, _, default).
:- test.
