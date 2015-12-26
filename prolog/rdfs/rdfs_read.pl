:- module(
  rdfs_read,
  [
    rdfs_class/2, % ?Class:rdf_term
                  % ?Graph:iri
    rdfs_comment/2, % ?Subject:rdf_term
                    % ?Comment:atom
    rdfs_instance/2, % ?Instance:rdf_term
                     % ?Class:or([bnode,iri])
    rdfs_label/2, % +Subject, -Label
    rdfs_label/3, % +Subject, +LanguagePriorityList, -Label
    rdfs_label/4, % +Subject, +LanguagePriorityList, -LanguageTag, -Label
    rdfs_label/5 % +Subject:rdf_term
                 % +LanguagePriorityList:or([atom,list(atom)])
                 % -LanguageTag:atom
                 % -Label:atom
                 % ?Graph:iri
  ]
).

/** <module> RDFS Read

@author Wouter Beek
@version 2015/08-2015/09, 2015/12
*/

:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).

:- rdf_meta(rdfs_class(o,r)).
:- rdf_meta(rdfs_comment(o,?)).
:- rdf_meta(rdfs_instance(o,r)).
:- rdf_meta(rdfs_label(o,-)).
:- rdf_meta(rdfs_label(o,+,-)).
:- rdf_meta(rdfs_label(o,+,-,-)).
:- rdf_meta(rdfs_label(o,+,-,-,r)).





%! rdfs_class(?Class:rdf_term, ?Graph:iri) is nondet.
% @tbd

rdfs_class(C, G):-
  rdf(C, rdf:type, rdfs:'Class', G).
rdfs_class(C, G):-
  rdf(_, rdf:type, C, G).
rdfs_class(C, G):-
  rdf(_, rdfs:domain, C, G).
rdfs_class(C, G):-
  rdf(_, rdfs:range, C, G).
rdfs_class(C, G):-
  rdf(C, rdfs:subClassOf, _, G).
rdfs_class(C, G):-
  rdf(_, rdfs:subClassOf, C, G).



%! rdfs_comment(?Subject:rdf_term, ?Comment:atom) is nondet.

rdfs_comment(S, Comm):-
  rdf_literal(S, rdfs:comment, xsd:string, Comm).



%! rdfs_instance(?Instance:rdf_term, ?Class:or([bnode,iri])) is nondet.
% @tbd

rdfs_instance(I, C):-
  rdf_expand_ct(rdf:type, P),
  rdf(I, P, C).



%! rdfs_label(+Subject:rdf_term, -Label:atom) is nondet.
% Wrapper around rdfs_label/3 using the global language priority list setting.

rdfs_label(S, Lex):-
  user:setting(language_priority_list, LRanges),
  rdfs_label(S, LRanges, Lex).


%! rdfs_label(
%!   +Subject:rdf_term,
%!   +LanguagePriorityList:list(atom),
%!   -Label:atom
%! ) is nondet.
% Wrapper around rdfs_label/4 not returning the language tag, if any.

rdfs_label(S, LRanges, Lex):-
  rdfs_label(S, LRanges, _, Lex).


%! rdfs_label(
%!   +Subject:rdf_term,
%!   +LanguagePriorityList:list(atom),
%!   -LanguageTag:atom,
%!   -Label:atom
%! ) is nondet.
% Wrapper around rdfs_label/5 with uninstantiated graph.

rdfs_label(S, LRanges, LTag, Lex):-
  rdfs_label(S, LRanges, LTag, Lex, _).


%! rdfs_label(
%!   +Subject:rdf_term,
%!   +LanguagePriorityList:list(atom),
%!   -LanguageTag:atom,
%!   -Label:atom,
%!   ?Graph:iri
%! ) is nondet.

rdfs_label(S, LRanges, LTag, Lex, G):-
  rdf_pref_string(S, rdfs:label, LRanges, LTag, Lex, G).
