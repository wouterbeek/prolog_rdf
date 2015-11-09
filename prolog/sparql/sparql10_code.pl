:- module(
  sparql10_code,
  [
    'BLANK_NODE_LABEL'//1, % -BlankNode:list(code)
    'PN_CHARS'//1, % ?Code:code
    'PN_CHARS_BASE'//1, % ?Code:code
    'PN_CHARS_U'//1, % ?Code:code
    'PN_LOCAL'//1, % ?Codes:list(code)
    'PN_PREFIX'//1, % ?Codes:list(code)
    'PNAME_LN'//1, % ?Codes:list(code)
    'PNAME_NS'//1 % ?Codes:list(code)
  ]
).

/** <module> SPARQL Query Language for RDF

@author Wouter Beek
@compat SPARQL Query Language for RDF
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(lists)).
:- use_module(library(sparql/sparql10_helpers)).





%! 'BLANK_NODE_LABEL'(-BlankNode:list(code))// .
% Blank node labels are written as `_:abc` for a blank node with label `abc`.
%
% The same blank node label cannot be used
% in two different basic graph patterns in the same query.
%
% ```ebnf
% [73] BLANK_NODE_LABEL ::= '_:'
%                           ( PN_CHARS_U | [0-9] )
%                           ( ( PN_CHARS | '.' )* PN_CHARS )?
% ```

'BLANK_NODE_LABEL'(L) --> "_:", !, blank_node_label_codes1(L).
blank_node_label_codes1([H|T]) --> 'PN_CHARS_U'(H), !, blank_node_label_codes2(T).
blank_node_label_codes1([H|T]) --> '[0-9]'(_, H), !, blank_node_label_codes2(T).
blank_node_label_codes2([0'.|T]) --> ".", !, blank_node_label_codes3(T).
blank_node_label_codes2([H|T]) --> 'PN_CHARS_U'(H), !, blank_node_label_codes4(T).
blank_node_label_codes2([]) --> "".
blank_node_label_codes3([0'.|T]) --> ".", !, blank_node_label_codes4(T).
blank_node_label_codes3([H|T]) --> 'PN_CHARS_U'(H), !, blank_node_label_codes4(T).
blank_node_label_codes4([0'.|T]) --> ".", !, blank_node_label_codes4(T).
blank_node_label_codes4([H|T]) --> 'PN_CHARS'(H), !, blank_node_label_codes4(T).
blank_node_label_codes4([]) --> "".



%! 'PN_CHARS'(?Code:code)// .
% ```
% PN_CHARS ::= PN_CHARS_U
%            | '-'
%            | [0-9]
%            | #xB7
%            | [#x300-#x36F]
%            | [#x203F-#x2040]
% ```

'PN_CHARS'(C) --> 'PN_CHARS_U'(C).
'PN_CHARS'(0'-) --> "-".
'PN_CHARS'(C) --> '[0-9]'(_, C).
'PN_CHARS'(C) --> code_radix(hex('00B7'), C).
'PN_CHARS'(C) --> between_code_radix(hex('0300'), hex('036F'), C).
'PN_CHARS'(C) --> between_code_radix(hex('203F'), hex('2040'), C).



%! 'PN_CHARS_BASE'(?Code:code)// .
% ```
% [95] PN_CHARS_BASE ::= [A-Z]
%                      | [a-z]
%                      | [#xC0-#xD6]
%                      | [#xD8-#xF6]
%                      | [#xF8-#x2FF]
%                      | [#x370-#x37D]
%                      | [#x37F-#x1FFF]
%                      | [#x200C-#x200D]
%                      | [#x2070-#x218F]
%                      | [#x2C00-#x2FEF]
%                      | [#x3001-#xD7FF]
%                      | [#xF900-#xFDCF]
%                      | [#xFDF0-#xFFFD]
%                      | [#x10000-#xEFFFF]
% ```
% @see Almost the same as XML 1.0.5 and 1.1.2,
%      but without colon and underscore.

'PN_CHARS_BASE'(C) --> '[A-Z]'(C).
'PN_CHARS_BASE'(C) --> '[a-z]'(C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('C0'), hex('D6'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('D8'), hex('F6'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('F8'), hex('2FF'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('370'), hex('37D'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('37F'), hex('1FFF'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('200C'), hex('200D'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('2070'), hex('218F'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('2C00'), hex('2FEF'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('3001'), hex('D7FF'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('F900'), hex('FDCF'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('FDF0'), hex('FFFD'), C).
'PN_CHARS_BASE'(C) --> between_code_radix(hex('10000'), hex('EFFFF'), C).



%! 'PN_CHARS_U'(?Code:code)// .
% ```
% [96] PN_CHARS_U ::= PN_CHARS_BASE | '_'
% ```
%
% Different from N-Quads and N-Triples where the colon is included as well.

'PN_CHARS_U'(C) --> 'PN_CHARS_BASE'(C).
'PN_CHARS_U'(0'_) --> "_".



%! 'PN_LOCAL'(?LocalPart:list(code))// is det.
% The **local part** of a prefixed name.
%
% ```
% [100] PN_LOCAL ::= ( PN_CHARS_U | [0-9] )
%                    (( PN_CHARS | '.')* PN_CHARS)?
% ```

'PN_LOCAL'(L) --> pn_local_codes1(L).
pn_local_codes1([H|T]) --> 'PN_CHARS_U'(H), !, pn_local_codes2(T).
pn_local_codes1([H|T]) --> '[0-9]'(_, H), !, pn_local_codes2(T).
pn_local_codes2([H|T]) --> 'PN_CHARS'(H), !, pn_local_codes3(T).
pn_local_codes2([0'.|T]) --> ".", !, pn_local_codes4(T).
pn_local_codes2([]) --> "".
pn_local_codes3([0'.|T]) --> ".", !, pn_local_codes4(T).
pn_local_codes3([H|T]) --> 'PN_CHARS'(H), !, pn_local_codes4(T).
pn_local_codes3([]) --> "".
pn_local_codes4([0'.|T]) --> ".", !, pn_local_codes4(T).
pn_local_codes4([H|T]) --> 'PN_CHARS'(H), !, pn_local_codes4(T).
pn_local_codes4([]) --> "".



%! 'PN_PREFIX'(?Codes:list(code))// .
% ```
% [99] PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
% ```

'PN_PREFIX'(L) --> pn_prefix_codes1(L).
pn_prefix_codes1([H|T]) --> 'PN_CHARS_BASE'(H), !, pn_prefix_codes2(T).
pn_prefix_codes2([0'.|T]) --> ".", !, pn_prefix_codes3(T).
pn_prefix_codes2([H|T]) --> 'PN_CHARS'(H), !, pn_prefix_codes4(T).
pn_prefix_codes2([]) --> "".
pn_prefix_codes3([0'.|T]) --> ".", !, pn_prefix_codes4(T).
pn_prefix_codes3([H|T]) --> 'PN_CHARS'(H), !, pn_prefix_codes4(T).
pn_prefix_codes4([0'.|T]) --> ".", !, pn_prefix_codes4(T).
pn_prefix_codes4([H|T]) --> 'PN_CHARS'(H), !, pn_prefix_codes4(T).
pn_prefix_codes4([]) --> "".



%! 'PNAME_LN'(?Codes:list(code))// .
% ```
% [72] PNAME_LN ::= PNAME_NS PN_LOCAL
% ```

'PNAME_LN'(L) --> 'PNAME_NS'(L1), 'PN_LOCAL'(L2), {append(L1, L2, L)}.



%! 'PNAME_NS'(?Codes:list(code))// .
% ```
% [71] PNAME_NS ::= PN_PREFIX? ':'
% ```

'PNAME_NS'(L) --> ('PN_PREFIX'(L0) ; {L0 = []}), ":", {append(L0, [0':], L)}.
