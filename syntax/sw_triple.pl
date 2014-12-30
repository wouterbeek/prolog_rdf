:- module(
  sw_triple,
  [
    triple//3 % ?Subject:or([bnode,iri])
              % ?Predicate:iri
              % ?Object:rdf_term
  ]
).

/** <module> SW: Triple

Grammar rules for triples.

@author Wouter Beek
@version 2014/12
*/

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).

:- use_module(plRdf(syntax/sw_term)).





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
