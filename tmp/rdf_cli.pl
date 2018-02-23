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
@version 2017/06-2017/09
*/

:- rdf_meta
   predicate(r),
   statement(r, r, o),
   statements(r, r, o).



%! predicate(?P) is nondet.

predicate(P) :-
  hdt_graph(Hdt, graph:default),
  hdt_predicate(Hdt, P).



%! statement(?S, ?P, ?O) is nondet.

statement(S, P, O) :-
  hdt(S, P, O),
  rdf_pp_triple(S, P, O),
  nl.



%! statements(?S, ?P, ?O) is nondet.

statements(S, P, O) :-
  findall(rdf(S,P,O), hdt(S, P, O), Triples),
  rdf_pp_triples(Triples).
