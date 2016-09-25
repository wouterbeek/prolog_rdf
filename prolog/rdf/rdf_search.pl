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
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(pl_term)).
:- use_module(library(porter_stem)).
:- use_module(library(q/q_rdf)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf11)).





%! rdf_search(+Pattern, -Lexs, -TotalNumLexs) is nondet.

rdf_search(Pattern, Lexs, TotalNumLexs) :-
  tokenize_atom(Pattern, Tokens),
  once(phrase(query(Q), Tokens)),
  rdf_find_literals(Q, Lexs),
  length(Lexs, TotalNumLexs).


query(Q) -->
  simple_query(Q1),
  (eos -> {Q = Q1} ; query(Q2), {Q = and(Q1,Q2)}).


simple_query(Token) --> ['"',Token,'"'], !.
simple_query(not(Token)) --> [-, Token].
simple_query(case(Token)) --> [Token].



%! rdf_search_result(+M, +Lexs, -Quad) is nondet.

rdf_search_result(M, Lexs, rdf(S,P,Lit,G)) :-
  member(Lex, Lexs),
  (   q(M, S, P, Lex^^D, G),
      Lit = Lex^^D
  ;   q(M, S, P, Lex@LTag, G),
      Lit = Lex@LTag
  ).
