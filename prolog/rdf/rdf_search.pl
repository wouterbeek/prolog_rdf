:- module(
  rdf_search,
  [
    rdf_search/3,       % +Pattern, -Lexs, -TotalNumLexs
    rdf_search_result/3 % +M, +Lexs, -Quad
  ]
).

/** <module> RDF search

@author Wouter Beek
@version 2016/07-2016/09
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pl_ext)).
:- use_module(library(porter_stem)).
:- use_module(library(q/q_rdf)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf11)).





%! rdf_search(+Pattern, -Lexs, -TotalNumLexs) is nondet.

rdf_search(Pattern, Lexs, TotalNumLexs) :-
  tokenize_atom(Pattern, Tokens1),
  hack_tokens(Tokens1, Tokens2),
  case_stem_sounds(Tokens2, [H|T]),
  (T == [] -> Query = H ; n_ary_term(and, [H|T], Query)),
  rdf_find_literals(Query, Lexs),
  length(Lexs, TotalNumLexs).


hack_tokens([], []) :- !.
hack_tokens([X], [X]) :- !.
hack_tokens([X,Y|_], [X,Y]) :- !.


case_stem_sounds([H|T1], [or(case(H),prefix(H))|T2]) :- !,
  case_stem_sounds(T1, T2).
case_stem_sounds([], []).



%! rdf_search_result(+M, +Lexs, -Quad) is nondet.

rdf_search_result(M, Lexs, rdf(S,P,Lit,G)) :-
  member(Lex, Lexs),
  (   q(M, S, P, Lex^^D, G),
      Lit = Lex^^D
  ;   q(M, S, P, Lex@LTag, G),
      Lit = Lex@LTag
  ).
