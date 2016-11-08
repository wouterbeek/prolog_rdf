:- module(
  rdf_search,
  [
    rdf_search/4,       % +Pattern, ?G, -Lexs, -NumTriples
    rdf_search/5,       % +Pattern, ?G, -Lexs, -NumLexs, -NumTriples
    rdf_search_result/4 % +M, ?G, +Lexs, -Quad
  ]
).

/** <module> RDF search

@author Wouter Beek
@version 2016/07-2016/11
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(pl_ext)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_stat)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_search(+, r, -, -),
   rdf_search(+, r, -, -, -),
   rdf_search_result(+, r, +, -).





%! rdf_search(+Pattern, ?G, -Lexs, -NumTriples) is nondet.
%! rdf_search(+Pattern, ?G, -Lexs, -NumLexs, -NumTriples) is nondet.

rdf_search(Pattern, G, Lexs, NumTriples) :-
  rdf_search(Pattern, G, Lexs, _, NumTriples).


rdf_search(Pattern, G, Lexs, NumLexs, NumTriples) :-
  string_phrase(pattern(Query), Pattern),
  rdf_find_literals(Query, Lexs),
  length(Lexs, NumLexs),
  maplist(lexical_form_number_of_triples0(G), Lexs, NumTripless),
  sumlist(NumTripless, NumTriples).

lexical_form_number_of_triples0(G, Lex, NumTriples) :-
  q_number_of_triples(trp, _, _, Lex@_, G, NumTriples1),
  q_number_of_triples(trp, _, _, Lex^^_, G, NumTriples2),
  NumTriples is NumTriples1 + NumTriples2.



%! rdf_search_result(+M, ?G, +Lexs, -Quad) is nondet.
%
% Succeeds if Quad is a quadruple in which a lexical form from Lexs
% appears.

rdf_search_result(M, G, Lexs, rdf(S,P,Lit,G)) :-
  member(Lex, Lexs),
  (   q(M, S, P, Lex^^D, G),
      Lit = Lex^^D
  ;   q(M, S, P, Lex@LTag, G),
      Lit = Lex@LTag
  ).





% GRAMMAR %

%! pattern(-Query)// is det.
%
% ```bnf
% pattern := literal "AND" pattern
% pattern := literal "OR" pattern
% pattern := literal
%
% literal := "NOT" token
% literal := token
%
% token := """ (^") """
% token := (^ *) "*"
% token := (^ )
% ```

pattern(and(A,B)) -->
  literal(A),
  keyword("AND"), !,
  pattern(B).
pattern(or(A,B)) -->
  literal(A),
  keyword("OR"), !,
  pattern(B).
pattern(A) -->
  literal(A).

literal(not(Token)) -->
  keyword("NOT"), !,
  must_see(token(Token)).
literal(Token) -->
  token(Token).

token(exact(Token)) -->
  "\"", !,
  ...(Cs),
  "\"",
  eot,
  {atom_codes(Token, Cs)},
  'ws*'.
token(prefix(Token)) -->
  ...(Cs),
  "*",
  eot, !,
  {atom_codes(Token, Cs)},
  'ws*'.
%token(case(Token)) -->
token(Token) -->
  ...(Cs),
  eot, !,
  {atom_codes(Token, Cs)},
  'ws*'.

keyword(Str) -->
  Str,
  must_see(eot),
  'ws*', !.

'ws*' --> ws, !, 'ws*'.
'ws*' --> "".

eot --> ws.
eot --> eos.
