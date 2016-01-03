:- module(
  rdf_dataset,
  [
    rdf_add_named_graph/2, % +Dataset:iri
                           % +NamedGraphMap:pair(or([bnode,iri]),atom)
    rdf_assert_dataset/2, % +DefaultGraph:atom
                          % +NamedGraphMap:list(pair(or([bnode,iri]),atom))
    rdf_dataset/1, % ?DefaultGraph:atom
    rdf_dataset/2, % ?DefaultGraph:atom
                   % ?NamedGraphMap:list(pair(or([bnode,iri]),atom))
    rdf_dataset_member/2 % ?Graph:atom
                         % ?Dataset:atom
  ]
).

/** <module> RDF Dataset

@author Wouter Beek
@compat RDF 1.1 Concepts and Format Syntax
@tbd RDF dataset merge.
@tbd Named graphs in an RDF dataset may share blank nodes.
@version 2013/09-2013/10, 2014/03, 2014/11, 2015/12
*/

:- use_module(library(error)).
:- use_module(library(lists)).

%! rdf_dataset(
%!   ?DefaultGraph:atom,
%!   ?NamedGraphs:list(pair(or([bnode,iri]),atom))
%! ) is nondet.

:- dynamic(rdf_dataset/2).





%! rdf_add_named_graph(
%!   +Dataset:iri,
%!   +NamedGraphMap:pair(or([bnode,iri]),atom)
%! ) is det.

rdf_add_named_graph(DG, Name-NG) :- !,
  retract(rdf_dataset(DG, Map1)),
  keysort([Name-NG|Map1], Map2),
  assert(rdf_dataset(DG, Map2)).
rdf_add_named_graph(DG, NG) :-
  rdf_add_named_graph(DG, NG-NG).



%! rdf_assert_dataset(
%!   +DefaultGraph:atom,
%!   +NamedGraphMap:list(pair(or([bnone,iri]),atom))
%! ) is det.
% @throws existence_error If the DefaultGraph or one of the named graphs
%                         does not exist.

rdf_assert_dataset(DG, _) :-
  \+ rdf_graph(DG), !,
  existence_error(rdf_graph, DG).
rdf_assert_dataset(_, NGMap) :-
  member(_-NG, NGMap),
  \+ rdf_graph(NG), !,
  existence_error(rdf_graph, NG).
rdf_assert_dataset(DG, NGMap) :-
  rdf_dataset(DG, NGMap0), !,
  (   NGMap0 \== NGMap
  ->  % You are trying to redefined DG.
  ;   % Fail silently
      fail
  ).
rdf_assert_dataset(DG, NGMap) :-
  assert(rdf_dataset(DG, NGMap)).



%! rdf_dataset(?DefaultGraph:atom) is semidet.

rdf_dataset(DG) :-
  rdf_dataset(DG, _).



%! rdf_dataset_member(+Graph:atom, +Dataset:atom) is semidet.
%! rdf_dataset_member(-Graph:atom, +Dataset:atom) is nondet.
%! rdf_dataset_member(+Graph:atom, -Dataset:atom) is nondet.
% Succeeds if Graph is part of Dataset.

% Default graph.
rdf_dataset_member(G, G) :-
  rdf_dataset(G, _).
% Named graph.
rdf_dataset_member(G, DG) :-
  rdf_dataset(DG, Map),
  memberchk(_-G, Map).
