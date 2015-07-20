:- module(
  rdf_view,
  [
    rdf_print_graph/1, % +Graph:atom
    rdf_print_quadruple/4, % ?Subject:or([bnode,iri])
                           % ?Predicate:iri
                           % ?Object:or([bnode,iri,literal])
                           % ?Graph:atom
    rdf_print_triple/4 % ?Subject:or([bnode,iri])
                       % ?Predicate:iri
                       % ?Object:or([bnode,iri,literal])
                       % ?Graph:atom
  ]
).

/** <module> RDF view

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_quoted)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_print_quadruple(r,r,o,?)).
:- rdf_meta(rdf_print_triple(r,r,o,?)).





rdf_print_graph(G):-
  rdf_print_triple(_, _, _, G),
  fail.
rdf_print_graph(_).

rdf_print_quadruple(S, P, O, G):-
  rdf(S, P, O, G),
  rdf_print_statement(S, P, O, G).

rdf_print_triple(S, P, O, G):-
  rdf(S, P, O, G),
  rdf_print_statement(S, P, O, _).


rdf_print_statement(S, P, O, G):-
  phrase(rdf_print_statement(S, P, O, G), Cs),
  string_codes(X, Cs),
  writeln(X).

rdf_print_statement(S, P, O, G) -->
  rdf_print_subject(S),
  " ",
  rdf_print_predicate(P),
  " ",
  rdf_print_object(O),
  (   {ground(G)}
  ->  rdf_print_graph(G)
  ;   ""
  ),
  " .".


rdf_print_subject(S) -->
  {rdf_is_bnode(S)}, !,
  rdf_print_bnode(S).
rdf_print_subject(S) -->
  rdf_print_iri(S).

rdf_print_predicate(P) -->
  rdf_print_iri(P).

rdf_print_object(O) -->
  {rdf_is_literal(O)}, !,
  rdf_print_literal(O).
rdf_print_object(O) -->
  {rdf_is_bnode(O)}, !,
  rdf_print_bnode(O).
rdf_print_object(O) -->
  rdf_print_iri(O).

rdf_print_graph(G) -->
  atom(G).


rdf_print_bnode(B) -->
  {atom_concat('__bnode', N, B)},
  "_:",
  atom(N).

rdf_print_iri(Global) -->
  {rdf_global_id(Prefix:Local, Global)}, !,
  atom(Prefix),
  ":",
  atom(Local).
rdf_print_iri(Global) -->
  bracketed(angular, atom(Global)).

rdf_print_literal(literal(type(D,L))) --> !,
  quoted(atom(L)),
  "^^",
  rdf_print_iri(D).
