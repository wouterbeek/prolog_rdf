:- module(
  ntriples11,
  [
    'EOL'//0,
    literal//1,   % ?Lit
    object//1,    % ?O
    predicate//1, % ?P
    subject//1,   % ?S
    triple//1,    % ?Triple
    ws//0
  ]
).
:- reexport(library(dcg/turtle11), [
     'IRIREF'//1,              % ?Iri:atom
     'STRING_LITERAL_QUOTE'//1 % ?Lex:atom
   ]).

/** <module> N-Triples 1.1

@author Wouter Beek
@compat N-Triples 1.1
@version 2015/11, 2016/03, 2016/12
*/

:- use_module(library(dcg_ext)).





%! 'EOL'// is det.
%
% ```ebnf
% EOL ::= [#xD#xA]+
% ```

'EOL' -->
  +(eol_code), !.

eol_code --> [0xD].
eol_code --> [0xA].



%! literal(?Lit)// is det.
%
% ```ebnf
% literal ::= STRING_LITERAL_QUOTE ( '^^' IRIREF | LANGTAG )?
% ```

literal(Lit) -->
  'STRING_LITERAL_QUOTE'(Lex),
  (   "^^"
  ->  'IRIREF'(D)
  ;   'LANGTAG'(LTag)
  ->  {rdf_equal(rdf:langString, D)}
  ;   {rdf_equal(xsd:string, D)}
  ),
  {q_literal(Lit, D, Lex, LTag)}.



%! object(?O)// is det.
%
% ```ebnf
% object ::= IRIREF | BLANK_NODE_LABEL | literal
% ```

object(Iri) -->
  'IRIREF'(Iri), !.
object(BNode) -->
  'BLANK_NODE_LABEL'(BNode), !.
object(Lit) -->
  literal(Lit).



%! predicate(?P)// is det.
%
% ```ebnf
% predicate ::= IRIREF
% ```

predicate(Iri) -->
  'IRIREF'(Iri).



%! subject(?S)// is det.
%
% ```ebnf
% subject ::= IRIREF | BLANK_NODE_LABEL
% ```

subject(Iri) -->
  'IRIREF'(Iri).
subject(BNode) -->
  'BLANK_NODE_LABEL'(BNode).



%! triple(?Triple)// is det.
%
% ```ebnf
% triple ::= subject predicate object '.'
% ```
%
% @compat N-Triples 1.1 [2].

triple(rdf(S,P,O)) -->
  subject(S),
  +(ws), !,
  predicate(P),
  +(ws), !,
  object(O),
  +(ws), !,
  ".".



%! ws// .
%
% White space is a sequence of:
%
%   - blanks (U+20)
%   - tabs (U+9)
%   - line feeds (U+A)
%   - carriage returns (U+D)
%   - comments

ws --> 'WS'.
ws --> comment.
ws --> "\t".
ws --> " ".
