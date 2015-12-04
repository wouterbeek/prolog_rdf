:- module(
  rdfs_read,
  [
    rdfs_class/2, % ?Class:rdf_term
                  % ?Graph:atom
    rdfs_comment/2, % ?Subject:rdf_term
                    % ?Comment:atom
    rdfs_instance/2, % ?Instance:rdf_term
                     % ?Class:or([bnode,iri])
    rdfs_label/4, % +Subject:rdf_term
                  % ?LanguagePriorityList:or([atom,list(atom)])
                  % -LanguageTag:atom
                  % -LexicalForm:atom
    rdfs_label/5 % +Subject:rdf_term
                 % ?LanguagePriorityList:or([atom,list(atom)])
                 % -LanguageTag:atom
                 % -LexicalForm:atom
                 % ?Graph:atom
  ]
).

/** <module> RDFS read

@author Wouter Beek
@version 2015/08-2015/09, 2015/12
*/

:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).

:- rdf_meta(rdfs_class(o,?)).
:- rdf_meta(rdfs_comment(o,?)).
:- rdf_meta(rdfs_instance(o,r)).
:- rdf_meta(rdfs_label(o,+,-,-)).
:- rdf_meta(rdfs_label(o,+,-,-,?)).





%! rdfs_class(?Class:rdf_term, ?Graph:atom) is nondet.
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



%! rdfs_label(
%!   +Subject:rdf_term,
%!   ?LanguagePriorityList:or([atom,list(atom)]),
%!   -LanguageTag:atom,
%!   -LexicalForm:atom
%! ) is nondet.

rdfs_label(S, LRanges, LTag, Lbl):-
  rdfs_label(S, LRanges, LTag, Lbl, _).


%! rdfs_label(
%!   +Subject:rdf_term,
%!   ?LanguagePriorityList:or([atom,list(atom)]),
%!   -LanguageTag:atom,
%!   -LexicalForm:atom,
%!   ?Graph:atom
%! ) is nondet.

rdfs_label(S, LRanges, LTag, Lex, G):-
  rdf_expand_ct(rdfs:label, P),
  (   % First look for language-tagged strings with matching language tag.
      rdf_langstring(S, P, LRanges, LTag-Lex, G)
  ;   % Secondly look for XSD strings with no language tag whatsoever.
      rdf_expand_ct(xsd:string, D),
      rdf_literal(S, P, D, Lex, G)
  ).
