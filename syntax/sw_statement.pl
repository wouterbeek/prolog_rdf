:- module(
  sw_statement,
  [
    statement//1, % ?Statement:compound
    triple//1 % ?Triple:compound
    
  ]
).

/** <module> SW: Statements

Grammar rules for RDF statements (i.e., triples or quadruples).

# N-Triples 1.1

```prolog
'*'(process_triple, [mode(parse),separator('EOL')]),
'?'('EOL', [mode(parse)]).
```

---

@author Wouter Beek
@version 2014/12-2015/01
*/

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).

:- use_module(plRdf(syntax/sw_char)).
:- use_module(plRdf(syntax/sw_term)).





%! statement(?Statement:compound)// .
% ```abnf
% statement ::= subject predicate object graphLabel? '.'
% ```
%
% @compat N-Quads 1.1 [2].

statement(Statement) -->
  subject(S),
  '+'(white_space(n), []),
  predicate(P),
  '+'(white_space(n), []),
  object(O),
  '+'(white_space(n), []),
  (   graphLabel(G),
      '+'(white_space(n), [])
  ->  {Statement = rdf(S,P,O,G)}
  ;   {Statement = rdf(S,P,O)}
  ),
  ".".



%! triple(?Triple:compound)// .
% ```abnf
% triple ::= subject predicate object '.'
% ```
%
% @compat N-Triples 1.1 [2].

triple(rdf(S,P,O)) -->
  subject(S),
  '+'(white_space(n), []),
  predicate(P),
  '+'(white_space(n), []),
  object(O),
  '+'(white_space(n), []),
  ".".
