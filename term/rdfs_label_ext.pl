:- module(
  rdfs_label_ext,
  [
    rdfs_list_by_label/3, % +RdfList:iri
                          % +LexicalForm:atom
                          % -Element:or([bnode,iri])
  ]
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
