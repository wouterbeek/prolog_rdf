:- module(
  z_print,
  [
    z_print/4, % ?S, ?P, ?O, ?G
    z_print/5  % ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Z print

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(pagination)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).
:- use_module(library(z/z_stmt)).

:- rdf_meta
   z_print(r, r, o, r),
   z_print(r, r, o, r, +).





%! z_print(?S, ?P, ?O, ?G) is nondet.
%! z_print(?S, ?P, ?O, ?G, +Opts) is nondet.

z_print(S, P, O, G) :-
  z_print(S, P, O, G, _{}).


z_print(S, P, O, G, Opts) :-
  pagination(Triple, z_triple(S, P, O, G, Triple), Opts, Result),
  pagination_result(
    Result,
    {Opts}/[Results]>>rdf_print_triples(Results, Opts)
  ).
