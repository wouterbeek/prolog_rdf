:- module(
  rdfs_read,
  [
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
@version 2015/08-2015/09
*/

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(owl/owl_read)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta(rdfs_comment(o,?)).
:- rdf_meta(rdfs_instance(o,r)).
:- rdf_meta(rdfs_label(o,+,-,-)).
:- rdf_meta(rdfs_label(o,+,-,-,?)).





%! rdfs_comment(?Subject:rdf_term, ?Comment:atom) is nondet.

rdfs_comment(S, Comm):-
  rdf_literal(S0, rdfs:comment, xsd:string, Comm),
  owl_id(S, S0).



%! rdfs_instance(?Instance:rdf_term, ?Class:or([bnode,iri])) is nondet.
% @tbd

rdfs_instance(I, C):-
  user:rdf(I, rdf:type, C).



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
  rdf_global_id(rdfs:label, P),
  (   % First look for language-tagged strings with matching language tag.
      rdf_langstring(S, P, LRanges, LTag-Lex, G)
  ;   % Secondly look for XSD strings with no language tag whatsoever.
      rdf_global_id(xsd:string, D),
      rdf_literal(S, P, D, Lex, G)
  ).
