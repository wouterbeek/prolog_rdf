:- module(
  sw_term,
  [
    object//1, % ?Object:rdf_term
    predicate//1, % ?Predicate:iri
    subject//1, % ?Subject:or([bnode,iri])
    triple//3 % ?Subject:or([bnode,iri])
              % ?Predicate:iri
              % ?Object:rdf_term
  ]
).

/** <module> SW: Terms

Various grammar rules for SW terms.

@author Wouter Beek
@version 2014/12
*/

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).

:- use_module(plRdf(syntax/sw_bnode)).
:- use_module(plRdf(syntax/sw_iri)).
:- use_module(plRdf(syntax/sw_literal)).





%! object(?Object:rdf_term)// .
% ```abnf
% object ::= IRIREF | BLANK_NODE_LABEL | literal
% ```
%
% @compat N-Triples [5].

object(Iri) -->
  'IRIREF'(Iri).
object(BNode) -->
  'BLANK_NODE_LABEL'(ntriples, BNode).
object(Literal) -->
  literal(Literal).



%! predicate(?Predicate:iri)// .
% ```abnf
% predicate 	::= 	IRIREF
% ```
%
% @compat N-Triples 1.1 [4].

predicate(Iri) -->
  'IRIREF'(Iri).



%! subject(?Subject:or([bnode,iri])) .
% ```abnf
% subject ::= IRIREF | BLANK_NODE_LABEL
% ```
%
% @compat N-Triples 1.1 [3].

subject(Iri) -->
  'IRIREF'(Iri).
subject(BNode) -->
  'BLANK_NODE_LABEL'(ntriples, BNode).



%! triple(?Subject:or([bnode,iri]), ?Predicate:iri, ?Object:rdf_term)// .
% ```abnf
% triple ::= subject predicate object '.'
% ```
%
% @compat N-Triples 1.1 [2].

triple(S, P, O) -->
  subject(S),
  '+'(ws, []),
  predicate(P),
  '+'(ws, []),
  object(O),
  '+'(ws, []),
  ".".

ws --> horizontal_tab.
ws --> space.
