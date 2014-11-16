:- module(
  rdfs_label_ext,
  [
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

:- use_module(library(semweb/rdf_db)). % RDF-meta.
:- use_module(library(semweb/rdfs)).

:- use_module(plDcg(dcg_collection)). % Meta-called.
:- use_module(plDcg(dcg_content)). % Meta-called.
:- use_module(plDcg(dcg_generics)).

:- use_module(plRdf(term/rdf_list)).
:- use_module(plRdf(term/rdf_langstring)).
:- use_module(plRdf(term/rdf_literal_build)).
:- use_module(plRdf(term/rdf_string)).

:- rdf_meta(rdfs_label(r,?,?,?)).
:- rdf_meta(rdfs_list_by_label(r,+,-)).
:- rdf_meta(rdfs_preferred_label(+,r,-,-,?)).
:- rdf_meta(rdfs_retractall_label(r,?,?,?)).



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
