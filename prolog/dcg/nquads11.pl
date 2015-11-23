:- module(
  nquads11,
  [
    graphLabel//1, % ?Graph:atom
    statement//1 % ?Statement:compound
  ]
).
:- reexport(library(dcg/ntriples11), [
     'EOL'//0,
     object//1, % ?Object:rdf_term
     predicate//1, % ?Predicate:iri
     subject//1, % ?Subject:or([bnode,iri])
     'UCHAR'//1, % ?Code:code
     ws//0
   ]).
:- reexport(library(dcg/turtle11_token), [
     'IRIREF'//1, % ?Iri:atom
     'STRING_LITERAL_QUOTE'//1 % ?String:atom
   ]).

/** <module> N-Quads 1.1: Codes

@author Wouter Beek
@compat N-Quads 1.1
@version 2015/11
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
