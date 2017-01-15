:- use_module(library(lists)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(service/ll_api)).

:- initialization(prefix).





prefix:-
  prefix('http://dbpedia.org/').


prefix(Prefix):-
  term_to_doc(Prefix, Doc),
  ldf(S, P, O, Doc),
  once((
    (   X = S
    ;   X = P
    ;   (O = literal(_) -> q_literal_datatype(O, X) ; X = O)
    ),
    atom_prefix(X, Prefix)
  )),
  rdf_print_triple(S, P, O),
  fail.
