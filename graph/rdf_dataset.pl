:- module(
  rdf_dataset,
  [
    rdf_assert_dataset/2, % +DefaultGraph:atom
                          % +NamedGraphMap:list(pair(or([bnode,iri]),atom))
    rdf_dataset/2 % ?DefaultGraph:atom
                  % ?NamedGraphMap:list(pair(or([bnode,iri]),atom))
  ]
).

/** <module> RDF Dataset

@author Wouter Beek
@compat RDF 1.1 Concepts and Format Syntax
@tbd RDF dataset merge.
@tbd Named graphs in an RDF dataset may share blank nodes.
@version 2013/09-2013/10, 2014/03, 2014/11
*/

:- use_module(library(error)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(db_ext)).

%! rdf_dataset(
%!   ?DefaultGraph:atom,
%!   ?NamedGraphs:list(pair(or([bnode,iri]),atom))
%! ) is nondet.

:- dynamic(rdf_dataset/2).





%! rdf_assert_dataset(
%!   +DefaultGraph:atom,
%!   +NamedGraphMap:list(pair(or([bnone,iri]),atom))
%! ) is det.
% @throws existence_error If the DefaultGraph or one of the named graphs
%                         does not exist.

rdf_assert_dataset(DG, _):-
  \+ rdf_graph(DG), !,
  existence_error(rdf_graph, DG).
rdf_assert_dataset(_, NGMap):-
  member(_-NG, NGMap),
  \+ rdf_graph(NG), !,
  existence_error(rdf_graph, NG).
rdf_assert_dataset(DG, NGMap):-
  db_add_novel(dataset(DG, NGMap)).
