:- module(
  sparql10,
  [
    'ANON'//1, % ?BlankNode:bnode
    'BlankNode'//1, % ?BlankNode:bnode
    'PrefixedName'//1 % ?Iri:atom
  ]
).

/** <module> SPARQL 1.0

@author Wouter Beek
compat SPARQL 1.0
@version 2015/11
*/

:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_re)).
:- use_module(library(semweb/rdf_db)).





%! 'ANON'(-BlankNode:bnode)// is det.
% A blank node that is used in only one place in the query syntax
% can be indicated with the notation `[]`.
%
% A unique blank node will be used to form the triple pattern.
%
% ```ebnf
% ANON ::= '[' WS* ']'
% ```

'ANON'(BNode) --> "[", *('WS') "]", {rdf_bnode(BNode)}.



%! 'BlankNode'(?BlankNode:bnode)// .
% Blank nodes are indicated by either the label form,
% such as `_:abc`, or the abbreviated form `[]`.
%
% ```ebnf
% BlankNode ::= BLANK_NODE_LABEL | ANON
% ```

'BlankNode'(BNode) --> 'BLANK_NODE_LABEL'(BNode).
'BlankNode'(BNode) --> 'ANON'(BNode).



%! 'PN_CHARS'(?Code:code)// .
% ```
% PN_CHARS ::= PN_CHARS_U
%            | '-'
%            | [0-9]
%            | #x00B7
%            | [#x300-#x36F]
%            | [#x203F-#x2040]
% ```

'PN_CHARS'(C)      --> 'PN_CHARS_U'(C).
'PN_CHARS'(0'-)    --> "-".
'PN_CHARS'(C)      --> digit(C).
'PN_CHARS'(0x00BF) --> [0x00B7].
'PN_CHARS'(C)      --> [C], {between(0x0300, 0x036F, C)}.
'PN_CHARS'(C)      --> [C], {between(0x203F, 0x2040, C)}.



%! 'PN_CHARS_BASE'(?Code:code)// .
% ```
% PN_CHARS_BASE ::= [A-Z]
%                 | [a-z]
%                 | [#x00C0-#x00D6]
%                 | [#x00D8-#x00F6]
%                 | [#x00F8-#x02FF]
%                 | [#x0370-#x037D]
%                 | [#x037F-#x1FFF]
%                 | [#x200C-#x200D]
%                 | [#x2070-#x218F]
%                 | [#x2C00-#x2FEF]
%                 | [#x3001-#xD7FF]
%                 | [#xF900-#xFDCF]
%                 | [#xFDF0-#xFFFD]
%                 | [#x10000-#xEFFFF]
% ```
% @see Almost the same as XML 1.0.5 and 1.1.2,
%      but without colon and underscore.

'PN_CHARS_BASE'(C) --> ascii_alpha(C).
'PN_CHARS_BASE'(C) --> [C], {between(0x00C0, 0x00D6).
'PN_CHARS_BASE'(C) --> [C], {between(0x00D8, 0x00F6).
'PN_CHARS_BASE'(C) --> [C], {between(0x00F8, 0x02FF).
'PN_CHARS_BASE'(C) --> [C], {between(0x0370, 0x037D).
'PN_CHARS_BASE'(C) --> [C], {between(0x037F, 0x1FFF).
'PN_CHARS_BASE'(C) --> [C], {between(0x200C, 0x200D).
'PN_CHARS_BASE'(C) --> [C], {between(0x2070, 0x218F).
'PN_CHARS_BASE'(C) --> [C], {between(0x2C00, 0x2FEF).
'PN_CHARS_BASE'(C) --> [C], {between(0x3001, 0xD7FF).
'PN_CHARS_BASE'(C) --> [C], {between(0xF900, 0xFDCF).
'PN_CHARS_BASE'(C) --> [C], {between(0xFDF0, 0xFFFD).
'PN_CHARS_BASE'(C) --> [C], {between(0x10000, 0xEFFFF).



%! 'PNAME_LN'(?PrefixedIri:compound)// .
% ```bnf
% PNAME_LN ::= PNAME_NS PN_LOCAL
% ```

'PNAME_LN'(Iri) -->
  'PNAME_NS'(Prefix),
  'PN_LOCAL'(Local),
  {rdf_global_id(Prefix:Local, Iri)}.



%! 'PNAME_NS'(?Prefix:atom)// is det.
% An IRI prefix label.
%
% Notice that the empty string is also a prefix label.
%
% ```bnf
% PNAME_NS ::= PN_PREFIX? ':'
% ```

'PNAME_NS'(Prefix) --> 'PN_PREFIX'(Prefix), ":".
'PNAME_NS'('')     --> ":".



%! 'PrefixedName'(?Iri:atom)// is det.
% A **prefixed name** is a *prefix label* and a *local part*,
% separated by a colon.
% The prefixed name is mapped to an IRI
% by concatenating the IRI associated by the prefix
% and the local part.
%
% ```bnf
% PrefixedName ::= PNAME_LN | PNAME_NS
% ```

'PrefixedName'(Iri) --> 'PNAME_LN'(Iri).
'PrefixedName'(Iri) --> 'PNAME_NS'(Prefix), {rdf_global_id(Prefix:'', Iri)}.
