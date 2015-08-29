:- module(
  rdfs_read,
  [
    rdfs_comment/2, % ?Subject:rdf_term
                    % ?Comment:atom
    rdfs_instance/2, % ?Instance:rdf_term
                     % ?Class:or([bnode,iri])
    rdfs_label/4, % +Subject:rdf_term
                  % ?LanguagePreference:atom
                  % -Language:atom
                  % -LexicalForm:atom
    rdfs_label/5 % +Subject:rdf_term
                 % ?LanguagePreference:atom
                 % -Language:atom
                 % -LexicalForm:atom
		 % ?Graph:atom
  ]
).

/** <module> RDFS read

@author Wouter Beek
@version 2015/08
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

rdfs_instance(I0, C):-
  rdf_is_literal(I0), !,
  bnode_literal(I, I0),
  rdfs_instance(I, C).
rdfs_instance(I, C):-
  rdfs_individual_of(I, C).



%! rdfs_label(
%!   +Subject:rdf_term,
%!   ?LanguagePreference:list(atom),
%!   -Language:list(atom),
%!   -LexicalForm:atom
%! ) is nondet.

rdfs_label(S, Pref, Lang, Lbl):-
  rdfs_label(S, Pref, Lang, Lbl, _).

%! rdfs_label(
%!   +Subject:rdf_term,
%!   ?LanguagePreference:atom,
%!   -Language:atom,
%!   -LexicalForm:atom,
%!   ?Graph:atom
%! ) is nondet.

% First look for language-tagged strings with matching language tag.
rdfs_label(S, Pref, Lang, Lex, G):-
  rdf_langstring(S, rdfs:label, Pref, Lang-Lex, G).
% Secondly look for XSD strings with no language tag.
rdfs_label(S, _, _, Lex, G):-
  rdf_literal(S, rdfs:label, xsd:string, Lex, G).
