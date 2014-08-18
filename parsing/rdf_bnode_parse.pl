:- module(
  rdf_bnode_parse,
  [
    'ANON'//1, % -BNode:bnode
    'BLANK_NODE_LABEL'//1, % -BNode:bnode
    'BlankNode'//1 % ?BNode:bnode
  ]
).

/** <module> RDF Blank Node Parse

Parser for RDF Blank Nodes.

@author Wouter Beek
@compat SPARQL 1.0 Query.
@compat SPARQL 1.1 Query.
@compat Turtle 1.1.
@version 2014/08
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plDcg(dcg_content)).



%! 'ANON'(-BNode:bnode)// is det.
% A blank node that is used in only one place in the query syntax
% can be indicated with [].
%
% A unique blank node will be used to form the triple pattern.
%
% ~~~{.ebnf}
% ANON ::= '[' WS* ']'
% ~~~
%
% @compat SPARQL 1.0 [162].
% @compat SPARQL 1.1 Query [163].
% @compat Turtle 1.1 [162s].

'ANON'(BNode) -->
  bracketed(square, 'WS*'),
  {rdf_bnode(Bnode)}.



%! 'BLANK_NODE_LABEL'(-BNode:bnode)// .
% Blank node labels are written as `_:abc` for a blank node with label `abc`.
%
% The same blank node label cannot be used
% in two different basic graph patterns in the same query.
%
% ~~~{.ebnf}
% BLANK_NODE_LABEL ::= '_:'
%                      ( PN_CHARS_U | [0-9] )
%                      ( ( PN_CHARS | '.' )* PN_CHARS )?
% ~~~
%
% @compat SPARQL 1.0 [141].
% @compat SPARQL 1.1 Query [142].
% @compat Turtle 1.1 [141s].

'BLANK_NODE_LABEL'(BNode) -->
  dcg_atom_codes('BLANK_NODE_LABEL_codes', BNodeLabel),
  (
    % The blank node label is already associated with a blank node.
    bnode_memory(BNodeLabel, BNode), !
  ;
    % A new blank node is created, and is associated to the blank node label.
    rdf_bnode(BNode),
    assert(bnode_memory(BNodeLabel, BNode))
  ).

'BLANK_NODE_LABEL_codes'([H|T2]) -->
  "_:",
  % First character after colon.
  (
    'PN_CHARS_U'(H)
  ;
    decimal_digit(H)
  ),
  % Rest of characters.
  (
    "", {T2 = []}
  ;
    'BLANK_NODE_LABEL_1*'(T1),
    'PN_CHARS'(Last),
    {append(T1, [Last], T2)}
  ).

'BLANK_NODE_LABEL_1*'([H|T]) -->
  ('PN_CHARS'(H) ; dot(H)),
  'BLANK_NODE_LABEL_1*'(T).
'BLANK_NODE_LABEL_1*'([]) --> [].



%! 'BlankNode'(?BNode:bnode)// .
% Blank nodes are indicated by either the label form,
% such as `_:abc`, or the abbreviated form `[]`.
%
% ~~~{.ebnf}
% BlankNode ::= BLANK_NODE_LABEL | ANON
% ~~~
%
% @compat SPARQL 1.0 [137].
% @compat SPARQL 1.1 Query [138].
% @compat Turtle 1.1 [137s].

'BlankNode'(BNode) -->
  'BLANK_NODE_LABEL'(BNode).
'BlankNode'(BNode) -->
  'ANON'(BNode).



%! name(?Codes:list(code))// .
% ~~~{.ebnf}
% name ::= nameStartChar nameChar*
% ~~~
%
% @compat Turtle 1.0 [32].
% @deprecated

name([H|T]) -->
  nameStartChar(H),
  'nameChar*'(T).

'nameChar*'([H|T]) -->
  nameChar(H),
  'nameChar*'(T).
'nameChar*'([]) --> [].



%! nameChar(?Code:code)// .
% ~~~{.ebnf}
% nameChar ::=   nameStartChar
%              | '-'
%              | [0-9]
%              | #x00B7
%              | [#x0300-#x036F]
%              | [#x203F-#x2040]
% ~~~
%
% @compat Turtle 1.0 [31].
% @deprecated

nameChar(C) --> nameStartChar(C).
nameChar(C) --> hyphen(C).
nameChar(C) --> decimal_digit(C).
nameChar(C) --> hex_code('00B7', C).
nameChar(C) --> between_hex('0300', '036F', C).
nameChar(C) --> between_hex('203F', '2040', C).



%! nameStartChar(?Code:code)// .
% ~~~{.ebnf}
% nameStartChar ::=   [A-Z]
%                   | "_" 
%                   | [a-z]
%                   | [#x00C0-#x00D6]
%                   | [#x00D8-#x00F6]
%                   | [#x00F8-#x02FF]
%                   | [#x0370-#x037D]
%                   | [#x037F-#x1FFF]
%                   | [#x200C-#x200D]
%                   | [#x2070-#x218F]
%                   | [#x2C00-#x2FEF]
%                   | [#x3001-#xD7FF]
%                   | [#xF900-#xFDCF]
%                   | [#xFDF0-#xFFFD]
%                   | [#x10000-#xEFFFF]
% ~~~
%
% @compat Turtle 1.0 [30].

nameStartChar(C) --> ascii_letter_uppercase(C).
nameStartChar(C) --> underscore(C).
nameStartChar(C) --> ascii_letter_lowercase(C).
nameStartChar(C) --> between_hex('00C0', '00D6').
nameStartChar(C) --> between_hex('00D8', '00F6').
nameStartChar(C) --> between_hex('00F8', '02FF').
nameStartChar(C) --> between_hex('0370', '037D').
nameStartChar(C) --> between_hex('037F', '1FFF').
nameStartChar(C) --> between_hex('200C', '200D').
nameStartChar(C) --> between_hex('2070', '218F').
nameStartChar(C) --> between_hex('2C00', '2FEF').
nameStartChar(C) --> between_hex('3001', 'D7FF').
nameStartChar(C) --> between_hex('F900', 'FDCF').
nameStartChar(C) --> between_hex('FDF0', 'FFFD').
nameStartChar(C) --> between_hex('10000', 'EFFFF').



%! nodeID(-BNode:bnode)// .
% ~~~{.ebnf}
% nodeID ::= '_:' name
% ~~~
%
% @compat Turtle 1.0 [26].
% @deprecated

nodeID(BNode) -->
  "_:",
  name(BNodeLabel),
  

