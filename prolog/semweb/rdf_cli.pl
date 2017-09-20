:- module(
  rdf_cli,
  [
    statement/3, % ?S, ?P, ?O
    statements/3 % ?S, ?P, ?O
  ]
).
:- reexport(library(semweb/rdf_ext)).
:- reexport(library(semweb/rdf_print)).

/** <module> RDF CLI

@author Wouter Beek
@version 2017/06
*/

:- rdf_meta
   statement(r, r, o),
   statements(r, r, o).





%! statement(?S, ?P, ?O) is nondet.

statement(S, P, O) :-
  hdt(S, P, O),
  rdf_pp_triple(S, P, O).



%! statements(?S, ?P, ?O) is nondet.

statements(S, P, O) :-
  findall(rdf(S,P,O), hdt(S, P, O), Triples),
  rdf_pp_triples(Triples).
