:- module(
  sparql11,
  [
  ]
).
:- reexport(library(dcg/sparql10), [
     'ANON'//1, % ?BlankNode:bnode
     'BlankNode'//1, % ?BlankNode:bnode
     'PNAME_LN'//1, % ?Iri:atom
     'PNAME_NS'//1, % ?Prefix:atom
     'PrefixName'//1 % ?Iri:atom
   ]).

/** <module> SPARQL 1.1

@author Wouter Beek
compat SPARQL 1.1
@version 2015/11
*/

:- use_module(library(dcg/basics)).





%! 'BLANK_NODE_LABEL'(-BlankNode:list(code))// .
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

'BLANK_NODE_LABEL'(A) --> "_:", !, dcg_atom(blank_node_label_codes1, A).
blank_node_label_codes1([H|T])   --> 'PN_CHARS_U'(H), !, blank_node_label_codes2(T).
blank_node_label_codes1([H|T])   --> digit(H),        !, blank_node_label_codes2(T).
blank_node_label_codes2([0'.|T]) --> ".",             !, blank_node_label_codes3(T).
blank_node_label_codes2([H|T])   --> 'PN_CHARS_U'(H), !, blank_node_label_codes4(T).
blank_node_label_codes2([])      --> "".
blank_node_label_codes3([0'.|T]) --> ".",             !, blank_node_label_codes4(T).
blank_node_label_codes3([H|T])   --> 'PN_CHARS_U'(H), !, blank_node_label_codes4(T).
blank_node_label_codes4([0'.|T]) --> ".",             !, blank_node_label_codes4(T).
blank_node_label_codes4([H|T])   --> 'PN_CHARS'(H),   !, blank_node_label_codes4(T).
blank_node_label_codes4([])      --> "".



