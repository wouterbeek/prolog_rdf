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
