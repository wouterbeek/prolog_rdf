:- module(
  nquads11,
  [
    graphLabel//1, % ?G
    statement//1 % ?Stmt
  ]
).
:- reexport(library(dcg/ntriples11), [
     'EOL'//0,
     object//1, % ?O
     predicate//1, % ?P
     subject//1, % ?S
     ws//0
   ]).
:- reexport(library(dcg/turtle11), [
     'IRIREF'//1, % ?Iri
     'STRING_LITERAL_QUOTE'//1 % ?String:atom
   ]).

/** <module> N-Quads 1.1

@author Wouter Beek
@compat N-Quads 1.1
@version 2015/11, 2016/03
*/

:- use_module(library(dcg/dcg_ext)).





%! graphLabel(?Graph:atom)// .
% ```abnf
% graphLabel ::= IRIREF | BLANK_NODE_LABEL
% ```

graphLabel(Iri)   --> 'IRIREF'(Iri).
graphLabel(BNode) --> 'BLANK_NODE_LABEL'(BNode).



%! statement(?Statement:compound)// .
% ```abnf
% statement ::= subject predicate object graphLabel? '.'
% ```

statement(Stmt) -->
  subject(S), +(ws), predicate(P), +(ws), object(O), +(ws),
  (graphLabel(G) -> +(ws), {Stmt = rdf(S,P,O,G)} ; {Stmt = rdf(S,P,O)}),
  ".".
