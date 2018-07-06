:- module(
  rdf_cli,
  [
    predicate/1, % ?P
    statement/3, % ?S, ?P, ?O
    statements/3 % ?S, ?P, ?O
  ]
).

/** <module> RDF CLI

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(semweb/hdt_db)).
:- use_module(library(semweb/hdt_dataset)).

:- rdf_meta
   predicate(r),
   statement(r, r, o),
   statements(r, r, o).





%! predicate(?P) is nondet.

predicate(P) :-
  hdt(Hdt),
  hdt_predicate(Hdt, P).



%! statement(?S, ?P, ?O) is nondet.

statement(S, P, O) :-
  statement_(S, P, O),
  rdf_pp_triple(S, P, O),
  nl.

statement_(S, P, O) :-
  hdt(Hdt),
  hdt_triple(Hdt, S, P, O).



%! statements(?S, ?P, ?O) is nondet.

statements(S, P, O) :-
  findall(rdf(S,P,O), statement_(S, P, O), Triples),
  rdf_pp_triples(Triples).
