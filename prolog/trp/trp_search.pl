:- module(
  trp_search,
  [
    trp_search/4,       % +Pattern, ?G, -Lexs, -NumTriples
    trp_search/5,       % +Pattern, ?G, -Lexs, -NumLexs, -NumTriples
    trp_search_result/4 % +M, ?G, +Lexs, -Quad
  ]
).

/** <module> In-memory RDF: Literal search

@author Wouter Beek
@version 2016/07-2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(pl_ext)).
:- use_module(library(trp/trp_stat)).
:- use_module(library(semweb/rdf_litindex)).
:- use_module(library(semweb/rdf11), [rdf_meta/1]).

:- rdf_meta
   trp_search(+, r, -, -),
   trp_search(+, r, -, -, -),
   trp_search_result(+, r, +, -).





%! trp_search(+Pattern, ?G, -Lexs, -NumTriples) is nondet.
%! trp_search(+Pattern, ?G, -Lexs, -NumLexs, -NumTriples) is nondet.

trp_search(Pattern, G, Lexs, NumTriples) :-
  trp_search(Pattern, G, Lexs, _, NumTriples).


trp_search(Pattern, G, Lexs, NumLexs, NumTriples) :-
  string_phrase(pattern(Query), Pattern),
  rdf_find_literals(Query, Lexs),
  length(Lexs, NumLexs),
  maplist(lexical_form_number_of_triples0(G), Lexs, NumTripless),
  sumlist(NumTripless, NumTriples).

lexical_form_number_of_triples0(G, Lex, NumTriples) :-
  trp_number_of_triples(_, _, Lex@_, G, NumTriples1),
  trp_number_of_triples(_, _, Lex^^_, G, NumTriples2),
  NumTriples is NumTriples1 + NumTriples2.



%! trp_search_result(?G, +Lexs, -Quad) is nondet.
%
% Succeeds if Quad is a quadruple in which a lexical form from Lexs
% appears.

trp_search_result(G, Lexs, rdf(S,P,Lit,G)) :-
  member(Lex, Lexs),
  (   rdf11:rdf(S, P, Lex^^D, G),
      Lit = Lex^^D
  ;   rdf11:rdf(S, P, Lex@LTag, G),
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
