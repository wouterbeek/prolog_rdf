:- module(
  rdf_deref,
  [
    rdf_deref/2 % +Iri, -Tuples
  ]
).

/** <module> RDF dereferencing

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(rdf/rdfio)).





%! rdf_deref(+Iri, -Tuples) is nondet.

rdf_deref(Iri, Tuples) :-
  rdf_load_tuples(Iri, Tuples).
