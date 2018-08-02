:- module(
  rdf_api,
  [
    triple/4,      % ?Backend, ?S, ?P, ?O
    triple_count/5 % ?Backend, ?S, ?P, ?O, -N
  ]
).

/** <module> RDF API

Backend-independent RDF API.

@author Wouter Beek
@version 2018
*/

:- use_module(library(semweb/rdf_prefix)).

:- multifile
    rdf_api:triple_/4,
    rdf_api:triple_count_/5.

:- rdf_meta
   triple(t, r, r, o),
   triple_count(t, r, r, o, -).




%! triple(?Backend, ?S:rdf_nonliteral, ?P:iri, ?O:rdf_term) is nondet.

triple(Backend, S, P, O) :-
  rdf_api:triple_(Backend, S, P, O).

rdf_api:triple_(dummy, _, _, _) :-
  fail.



%! triple_count(?Backend, ?S:rdf_nonliteral, ?P:iri, ?O:rdf_term, -N:nonneg) is nondet.

triple_count(Backend, S, P, O, N) :-
  rdf_api:triple_count_(Backend, S, P, O, N).

rdf_api:triple_count_(dummy, _, _, _, 0) :-
  fail.
