:- module(
  rdfs_label_ext,
  [
    rdfs_assert_label/3, % +Subject:oneof([bnode,iri])
                         % +LexicalForm:atom
                         % +RdfGraph:graph
    rdfs_assert_label/4, % +Subject:oneof([bnode,iri])
                         % +LexicalForm:atom
                         % +LangTag:atom
                         % +RdfGraph:graph
    rdfs_label/4, % ?Subject:or([bnode,iri])
                  % ?LexicalForm:atom
                  % ?LangTag:atom
                  % ?RdfGraph:atom
    rdfs_list_by_label/3, % +RdfList:iri
                          % +LexicalForm:atom
                          % -Element:or([bnode,iri])
    rdfs_preferred_label/5, % +LanguageTags:or([atom,list(atom)])
                            % +Subject:oneof([bnode,iri])
                            % -LexicalForm:atom
                            % -LangTag:atom
                            % ?RdfGraph:atom
    rdfs_retractall_label/4, % ?Subject:oneof([bnode,iri])
                             % ?LexicalForm:atom
                             % ?LangTag:atom
                             % ?RdfGraph:graph
    rdfs_update_label/3 % +Subject:or([bnode,iri])
                        % +LexicalForm:atom
                        % +RdfGraph:atom
  ]
).

/** <module> RDFS label extensions

Predicates for RDFS labels.

@author Wouter Beek
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07-2013/09,
         2014/01-2014/03
*/

:- use_module(dcg(dcg_collection)). % Meta-called.
:- use_module(dcg(dcg_content)). % Meta-called.
:- use_module(dcg(dcg_generic)).
:- use_module(library(semweb/rdf_db)). % RDF-meta.
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf_term(rdf_language_tagged_string)).
:- use_module(rdf_term(rdf_literal_build)).
:- use_module(rdf_term(rdf_string)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- rdf_meta(rdfs_assert_label(r,+,+)).
:- rdf_meta(rdfs_assert_label(r,+,+,+)).
:- rdf_meta(rdfs_label(r,?,?,?)).
:- rdf_meta(rdfs_list_by_label(r,+,-)).
:- rdf_meta(rdfs_preferred_label(+,r,-,-,?)).
:- rdf_meta(rdfs_retractall_label(r,?,?,?)).



%! rdfs_assert_label(
%!   +Subject:oneof([bnode,iri]),
%!   +LexicalForm:atom,
%!   +RdfGraph:atom
%! ) is det.
% Assert the subject's label description.
%
% @arg Subject An RDF subject term.
% @arg LexicalForm An atomic description of a resource.
% @arg RdfGraph The atomic name of an RDF graph.
% @see rdfs_assert_label/4 also specifies the label.

rdfs_assert_label(S, LexicalForm, G):-
  rdf_assert_string(S, rdfs:label, LexicalForm, G).


%! rdfs_assert_label(
%!   +Subject:oneof([bnode,iri]),
%!   +LexicalForm:atom,
%!   +LangTag:atom,
%!   +RdfGraph:atom
%! ) is det.
% Assert the subject's label description in the given label.
%
% @arg Subject An RDF subject term.
% @arg LexicalForm An atomic description of a resource.
% @arg LangTag The atomic name of a language.
% @arg RdfGraph The atomic name of an RDF graph.
%
% @tbd Why are explicit conversions necessary here?

rdfs_assert_label(S, LexicalForm, LangTag, G):-
  rdf_assert_language_tagged_string(S, rdfs:label, LexicalForm, LangTag, G).


%! rdfs_label(
%!   ?Subject:or([bnode,iri]),
%!   ?LexicalForm:atom,
%!   ?LangTag:atom,
%!   ?RdfGraph:atom
%! ) is nondet.
% Returns the RDFS label of the given resource in the given language tag,
% if it exists.
%
% Also returns listified labels for RDF list resources.

% An RDF list, compose the lexical form based on its members.
rdfs_label(S, LexicalForm, LangTag, G):-
  rdf_is_list(S), !,
  rdf_list([recursive(false)], S, RdfTerms),
  findall(
    RdfsLabel,
    (
      member(RdfTerm, RdfTerms),
      rdfs_label(RdfTerm, RdfsLabel, LangTag, G)
    ),
    RdfsLabels
  ),
  dcg_with_output_to(atom(LexicalForm), list(pl_term, RdfsLabels)).
% A language-tagged string.
rdfs_label(S, LexicalForm, LangTag, G):-
  rdf_language_tagged_string(S, rdfs:label, LexicalForm, LangTag, G).
% A string with no language tag.
rdfs_label(S, LexicalForm, _, G):-
  % @tbd This should not be necessary,
  rdf_global_id(rdfs:label, Dummy),
  rdf_string(S, Dummy, LexicalForm, G).


%! rdfs_list_by_label(
%!   +RdfList:or([bnode,iri]),
%!   +LexicalForm:atom,
%!   -Element:or([bnode,iri])
%! ) is nondet.
% Returns RDF list elements that have the given label.

rdfs_list_by_label(RdfList, LexicalForm, Element):-
  rdf_list_first(RdfList, First),
  rdfs_list_by_label_(First, LexicalForm, Element).

rdfs_list_by_label_(Element, LexicalForm, Element):-
  rdfs_label(Element, LexicalForm), !.
rdfs_list_by_label_(Element, LexicalForm, Element0):-
  rdf_list_next(Element, NextElement),
  rdfs_list_by_label_(NextElement, LexicalForm, Element0).


%! rdfs_preferred_label(
%!   +PreferredLanguageTags:or([atom,list(atom)]),
%!   ?Subject:or([bnode,iri]),
%!   -LexicalForm:atom,
%!   -LanguageTags:atom,
%!   ?Graph:atom
%! ) is nondet.
% Multiple labels are returned (nondet) in a descending preference order.

rdfs_preferred_label(LangTag, S, LexicalForm, LangTag, G):-
  \+ is_list(LangTag), !,
  rdfs_preferred_label([LangTag], S, LexicalForm, LangTag, G).
rdfs_preferred_label(LangTags, S, LexicalForm, LangTag, G):-
  member(LangTag, LangTags),
  rdfs_label(S, LexicalForm, LangTag, G), !.
rdfs_preferred_label(_, S, LexicalForm, LangTag, G):-
  rdfs_label(S, LexicalForm, LangTag, G).


%! rdfs_update_label(+Subject:or([bnode,iri]), +LexicalForm:atom, +RdfGraph:atom) is det.

rdfs_update_label(S, LexicalForm, G):-
  rdfs_retractall_label(S, _, _, G),
  rdfs_assert_label(S, LexicalForm, G).


%! rdfs_retractall_label(
%!   +Subject:or([bnode,iri]),
%!   ?LexicalForm:atom,
%!   ?LangTag:atom,
%!   ?RdfGraph:atom
%! ) is det.

rdfs_retractall_label(S, LexicalForm, LangTag, G):-
  rdf_retractall_literal(S, rdfs:label, LexicalForm, LangTag, G).

