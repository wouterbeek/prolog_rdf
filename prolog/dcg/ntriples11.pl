:- module(
  ntriples11,
  [
    'EOL'//0,
    literal//1, % ?Literal:compound
    object//1, % ?Object:rdf_term
    predicate//1, % ?Predicate:iri
    subject//1, % ?Subject:or([bnode,iri])
    triple//1, % ?Triple:compound
    ws//0
  ]
).
:- reexport(library(dcg/turtle11_code), [
     'UCHAR'//1 % ?Code:code
   ]).
:- reexport(library(dcg/turtle11_token), [
     'IRIREF'//1, % ?Iri:atom
     'STRING_LITERAL_QUOTE'//1 % ?String:atom
   ]).

/** <module> N-Triples 1.1: Codes

@author Wouter Beek
@compat N-Triples 1.1
@version 2015/11
*/

:- use_module(library(dcg/dcg_re)).





%! 'EOL'// .
% ```abnf
% EOL ::= [#xD#xA]+
% ```

'EOL' --> +(eol).
eol --> [0xD].
eol --> [0xA].



%! literal(?Literal:compound)// .
% ```ebnf
% literal ::= STRING_LITERAL_QUOTE ( '^^' IRIREF | LANGTAG )?
% ```

literal(Lit) -->
  'STRING_LITERAL_QUOTE'(Lex),
  (   "^^"
  ->  'IRIREF'(D), {Lit = literal(type(D,Lex))}
  ;   'LANGTAG'(LTag) -> {Lit = literal(lang(LTag,Lex))}
  ;   {Lit = literal(Lex)}
  ).



%! object(?Object:rdf_term)// .
% ```abnf
% object ::= IRIREF | BLANK_NODE_LABEL | literal
% ```

object(Iri)   --> 'IRIREF'(Iri).
object(BNode) --> 'BLANK_NODE_LABEL'(BNode).
object(Lit)   --> literal(Lit).



%! predicate(?Predicate:iri)// .
% ```abnf
% predicate ::= IRIREF
% ```

predicate(Iri) --> 'IRIREF'(Iri).



%! subject(?Subject:or([bnode,iri])) .
% ```abnf
% subject ::= IRIREF | BLANK_NODE_LABEL
% ```

subject(Iri)   --> 'IRIREF'(Iri).
subject(BNode) --> 'BLANK_NODE_LABEL'(BNode).



%! triple(?Triple:compound)// .
% ```abnf
% triple ::= subject predicate object '.'
% ```
%
% @compat N-Triples 1.1 [2].

triple(rdf(S,P,O)) -->
  subject(S), +(ws), predicate(P), +(ws), object(O), +(ws), ".".



%! ws// .
% White space is a sequence of:
%   - blanks (U+20)
%   - tabs (U+9)
%   - line feeds (U+A)
%   - carriage returns (U+D)
%   - comments

ws --> 'WS'.
ws --> comment.
ws --> "\t".
ws --> " ".
