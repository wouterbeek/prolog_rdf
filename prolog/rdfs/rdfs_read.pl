:- module(
  rdfs_read,
  [
    rdfs_class0/2,    % ?C, ?G
    rdfs_comment/2,   % ?S, ?Comment
    rdfs_instance0/2, % ?I, ?C
    rdfs_label/2,     % +S, -Label
    rdfs_label/3,     % +S, +LanguagePriorityList, -Label
    rdfs_label/4,     % +S, +LanguagePriorityList, -LTag, -Label
    rdfs_label/5,     % +S, +LanguagePriorityList, -LTag, -Label, ?G
    rdfs_one_label/2  % +S, -Label
  ]
).

/** <module> RDFS Read

@author Wouter Beek
@version 2015/08-2015/09, 2015/12-2016/01
*/

:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).

:- rdf_meta
   rdfs_class0(o, r),
   rdfs_comment(o, ?),
   rdfs_instance0(o, r),
   rdfs_label(o, -),
   rdfs_label(o, +, -),
   rdfs_label(o, +, -, -),
   rdfs_label(o, +, -, -, r),
   rdfs_one_label(o, -).





%! rdfs_class0(?C, ?G) is nondet.

rdfs_class0(C, G) :-
  rdf(C, rdf:type, rdfs:'Class', G).
rdfs_class0(C, G) :-
  rdf(_, rdf:type, C, G).
rdfs_class0(C, G) :-
  rdf(_, rdfs:domain, C, G).
rdfs_class0(C, G) :-
  rdf(_, rdfs:range, C, G).
rdfs_class0(C, G) :-
  rdf(C, rdfs:subClassOf, _, G).
rdfs_class0(C, G) :-
  rdf(_, rdfs:subClassOf, C, G).



%! rdfs_comment(?S, ?Comment:atom) is nondet.

rdfs_comment(S, Comm) :-
  rdf_literal(S, rdfs:comment, xsd:string, Comm).



%! rdfs_instance0(?I, ?C) is nondet.

rdfs_instance0(I, D) :-
  rdf_instance(I, C),
  rdf_reachable(C, rdfs:subClassOf, D).



%! rdfs_label(+S, -Label) is nondet.
% Wrapper around rdfs_label/3 using the global language priority list setting.

rdfs_label(S, Lex) :-
  user:setting(language_priority_list, LRanges),
  rdfs_label(S, LRanges, Lex).


%! rdfs_label(+S, +LanguagePriorityList, -Label) is nondet.
% Wrapper around rdfs_label/4 not returning the language tag, if any.

rdfs_label(S, LRanges, Lex) :-
  rdfs_label(S, LRanges, _, Lex).


%! rdfs_label(+S, +LanguagePriorityList, -LTag, -Label) is nondet.
% Wrapper around rdfs_label/5 with uninstantiated graph.

rdfs_label(S, LRanges, LTag, Lex) :-
  rdfs_label(S, LRanges, LTag, Lex, _).


%! rdfs_label(+S, +LanguagePriorityList, -LTag, -Label, ?G) is nondet.

rdfs_label(S, LRanges, LTag, Lex, G) :-
  rdf_pref_string(S, rdfs:label, LRanges, LTag, Lex, G).



%! rdfs_one_label(+S, -Label) is det.

rdfs_one_label(S, Lex) :-
  rdfs_label(S, Lex), !.
rdfs_one_label(_, '∅').
