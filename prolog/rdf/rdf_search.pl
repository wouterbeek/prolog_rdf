:- module(
  rdf_search,
  [
    rdf_search/3 % +M, +Pattern, -Quad
  ]
).

/** <module> RDF search

@author Wouter Beek
@version 2016/07-2016/08
*/

:- use_module(library(lists)).
:- use_module(library(pl_term)).
:- use_module(library(porter_stem)).
:- use_module(library(q/q_stmt)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_search(+, +, t).





%! rdf_search(+M, +Pattern, -Quad) is nondet.

rdf_search(M, Pattern, rdf(S,P,Lit,G)) :-
  tokenize_atom(Pattern, [H|T]),
  (T == [] -> Query = H ; n_ary_term(and, [H|T], Query)),
  rdf_find_literals(Query, Strs),
  member(Str, Strs),
  (   q(M, S, P, Str^^D, G),
      Lit = Str^^D
  ;   q(M, S, P, Str@LTag, G),
      Lit = Str@LTag
  ).
