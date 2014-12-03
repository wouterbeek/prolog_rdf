:- module(
  sw_bnode,
  [
    'ANON'//1, % -BNode:bnode
    'BLANK_NODE_LABEL'//1, % -BNode:bnode
    'BlankNode'//1, % ?BNode:bnode
    nodeID//2 % ?Language:oneof([manchester,turtle10])
              % ?BNode:bnode
  ]
).

/** <module> SW grammar: Blank nodes

Grammar rules for blank nodes in Semantic Web standards.

Blank nodes in graph patterns act as variables,
not as references to specific blank nodes in the data being queried.

@author Wouter Beek
@compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
@compat SPARQL 1.0 Query.
@compat SPARQL 1.1 Query.
@compat Turtle 1.1.
@version 2014/08-2014/10, 2014/12
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_bracket)).
:- use_module(plDcg(dcg_code)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_meta)).

:- use_module(plRdf(syntax/sw_char)).

%! bnode_memory(?BNodeLabel:atom, BNode:bnode) is nondet.

:- dynamic(bnode_memory/2).





%! 'ANON'(-BNode:bnode)// is det.
% A blank node that is used in only one place in the query syntax
% can be indicated with []
%
% A unique blank node will be used to form the triple pattern.
%
% ```ebnf
% ANON ::= '[' WS* ']'
% ```
%
% @compat SPARQL 1.0 [94]
% @compat SPARQL 1.1 Query [163]
% @compat Turtle 1.1 [162s]

'ANON'(BNode) -->
  bracketed(square, '*'('WS', [])),
  {rdf_bnode(BNode)}.



%! 'BLANK_NODE_LABEL'(-BNode:bnode)// .
% Blank node labels are written as `_:abc` for a blank node with label `abc`.
%
% The same blank node label cannot be used
% in two different basic graph patterns in the same query.
%
% ```ebnf
% BLANK_NODE_LABEL ::= '_:'
%                      ( PN_CHARS_U | [0-9] )
%                      ( ( PN_CHARS | '.' )* PN_CHARS )?
% ```
%
% @compat SPARQL 1.0 [73]
% @compat SPARQL 1.1 Query [142]
% @compat Turtle 1.1 [141s]

'BLANK_NODE_LABEL'(BNode) -->
  dcg_atom_codes('BLANK_NODE_LABEL_codes', BNodeLabel),
  {bnode_label(BNodeLabel, BNode)}.

'BLANK_NODE_LABEL_codes'([H|T]) -->
  "_:",
  
  % First character after colon.
  (   'PN_CHARS_U'(H)
  ;   decimal_digit(H)
  ),
  
  % Non-first characters.
  (   '*'('BLANK_NODE_LABEL_code', T0, []),
      'PN_CHARS'(Last),
      {append(T0, [Last], T)}
  ;   {T = []}
  ).

'BLANK_NODE_LABEL_code'(Code) --> 'PN_CHARS'(Code).
'BLANK_NODE_LABEL_code'(Code) --> dot(Code).



%! 'BlankNode'(?BNode:bnode)// .
% Blank nodes are indicated by either the label form,
% such as `_:abc`, or the abbreviated form `[]`.
%
% ```ebnf
% BlankNode ::= BLANK_NODE_LABEL | ANON
% ```
%
% @compat SPARQL 1.0 [69]
% @compat SPARQL 1.1 Query [138]
% @compat Turtle 1.1 [137s]

'BlankNode'(BNode) -->
  'BLANK_NODE_LABEL'(BNode).
'BlankNode'(BNode) -->
  'ANON'(BNode).



%! name(?Name:atom)// .
% ```ebnf
% name ::= nameStartChar nameChar*
% ```
%
% @compat Turtle 1.0 [32]
% @deprecated

name(Name) -->
  {var(Name)}, !,
  nameStartChar(H),
  '*'(nameChar, T, []),
  {atom_codes(Name, [H|T])}.
name(Name) -->
  {atom_codes(Name, [H|T])},
  nameStartChar(H),
  '*'(nameChar, T, []).



%! nameChar(?Code:code)// .
% ```ebnf
% nameChar ::=   nameStartChar
%              | '-'
%              | [0-9]
%              | #x00B7
%              | [#x0300-#x036F]
%              | [#x203F-#x2040]
% ```
%
% @compat Turtle 1.0 [31]
% @deprecated

nameChar(Code) --> nameStartChar(Code).
nameChar(Code) --> hyphen(Code).
nameChar(Code) --> decimal_digit(Code).
nameChar(Code) --> code_radix(hex('00B7'), Code).
nameChar(Code) --> between_code_radix(hex('0300'), hex('036F'), Code).
nameChar(Code) --> between_code_radix(hex('203F'), hex('2040'), Code).



%! nameStartChar(?Code:code)// .
% ```ebnf
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
% ```
%
% @compat Turtle 1.0 [30]
% @deprecated

nameStartChar(Code) --> letter_uppercase(Code).
nameStartChar(Code) --> underscore(Code).
nameStartChar(Code) --> letter_lowercase(Code).
nameStartChar(Code) --> between_code_radix(hex('00C0'), hex('00D6'), Code).
nameStartChar(Code) --> between_code_radix(hex('00D8'), hex('00F6'), Code).
nameStartChar(Code) --> between_code_radix(hex('00F8'), hex('02FF'), Code).
nameStartChar(Code) --> between_code_radix(hex('0370'), hex('037D'), Code).
nameStartChar(Code) --> between_code_radix(hex('037F'), hex('1FFF'), Code).
nameStartChar(Code) --> between_code_radix(hex('200C'), hex('200D'), Code).
nameStartChar(Code) --> between_code_radix(hex('2070'), hex('218F'), Code).
nameStartChar(Code) --> between_code_radix(hex('2C00'), hex('2FEF'), Code).
nameStartChar(Code) --> between_code_radix(hex('3001'), hex('D7FF'), Code).
nameStartChar(Code) --> between_code_radix(hex('F900'), hex('FDCF'), Code).
nameStartChar(Code) --> between_code_radix(hex('FDF0'), hex('FFFD'), Code).
nameStartChar(Code) --> between_code_radix(hex('10000'), hex('EFFFF'), Code).




%! nodeID(?Language:oneof([manchester,turtle10]), ?BNode:bnode)// .
% ```bnf
% [Manchester]   nodeID := a finite sequence of characters matching
%                          the BLANK_NODE_LABEL production of [SPARQL].
% [Turtle]       nodeID ::= '_:' name
% ```
%
% @compat OWL 2 Web Ontology Language Manchester Syntax (Second Edition)
% @compat Turtle 1.0 [26]
% @deprecated Turtle 1.1 [26]

nodeID(manchester, BNode) -->
  'BLANK_NODE_LABEL'(BNode).
nodeID(turtle, BNodeLabel) -->
  "_:",
  name(BNodeLabel).





% HELPERS

% The blank node label is already associated with a blank node.
bnode_label(BNodeLabel, BNode):-
  bnode_memory(BNodeLabel, BNode), !.
% A new blank node is created, and is associated to the blank node label.
bnode_label(BNodeLabel, BNode):-
  rdf_bnode(BNode),
  assert(bnode_memory(BNodeLabel, BNode)).
