:- module(
  sw_triple,
  [
    triple//1 % ?Triple:compound
  ]
).

/** <module> SW: Triple

Grammar rules for triples.

# N-Triples 1.1

```prolog
'*'(process_triple, [mode(parse),separator('EOL')]),
'?'('EOL', [mode(parse)]).
```

---

@author Wouter Beek
@version 2014/12
*/

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).

:- use_module(plRdf(syntax/sw_char)).
:- use_module(plRdf(syntax/sw_term)).





%! triple(?Triple:compound)// .
% ```abnf
% triple ::= subject predicate object '.'
% ```
%
% @compat N-Triples 1.1 [2].

triple(rdf(S,P,O)) -->
  subject(S),
  '+'(white_space(ntriples), []),
  predicate(P),
  '+'(white_space(ntriples), []),
  object(O),
  '+'(white_space(ntriples), []),
  ".".
