:- module(
  datacube_table,
  [
    datacube_table/3 % +DataSet:iri
                     % -HeaderRows:list(pair(term,positive_integer))
                     % -DataRows:list(list)
  ]
).

/** <module> Data Cube Table

Creates tables based on a Data Cube graph.

@author Wouter Beek
@version 2014/10
*/

:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3])).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(pair_ext)).
:- use_module(generics(sort_ext)).



%! datacube_table(
%!   +DataSet:iri,
%!   -HeaderRows:list(pair(term,positive_integer)),
%!   -DataRows:list(list)
%! ) is det.

datacube_table(DataSet, HeaderRows, DataRows):-
  datacube_tree(DataSet-DataSet, Tree),
  Tree = _-Subtrees,
  tree_headers(Subtrees, HeaderRows),
  tree_data(Tree, DataRows).


%! datacube_tree(+Root:pair, -Tree:compound) is det.

datacube_tree(Root1-Root2, Root2-Trees):-
  findall(
    Slice-DimensionValue,
    (
      rdf(Root1, qb:slice, Slice),
      rdf(Slice, Dimension, DimensionValue),
      rdf(_, qb:dimension, Dimension)
    ),
    Pairs
  ),
  Pairs \== [], !,
  maplist(datacube_tree, Pairs, Trees).
datacube_tree(Root1-Root2, Root2-Pairs):-
  findall(
    DimensionValue-MeasureValue,
    (
      rdf(Root1, qb:observation, Observation),
      rdf(Observation, Dimension, DimensionValue),
      rdf(_, qb:dimension, Dimension),
      rdf(Observation, Measure, MeasureValue),
      rdf(_, qb:measure, Measure)
    ),
    Pairs
  ).


%! leaf_node(+Tree:compound, -DimensionValue, -MeasureValue) is nondet.

leaf_node(DimensionValue-MeasureValue, DimensionValue, MeasureValue):-
  \+ is_list(MeasureValue), !.
leaf_node(_-Trees, DimensionValue, MeasureValue):-
  member(Tree, Trees),
  leaf_node(Tree, DimensionValue, MeasureValue).


%! tree_data(+Tree:compound, DataRows:list(list)) is det.

tree_data(Tree, DataRows):-
  findall(
    DimensionValue-MeasureValue,
    leaf_node(Tree, DimensionValue, MeasureValue),
    Pairs1
  ),
  sort(Pairs1, Pairs2, [duplicates(true),inverted(false)]),
  group_pairs_by_key(Pairs2, Groups),
  pairs_values(Groups, DataRows).


%! tree_headers(
%!   +Subtrees:list(compound),
%!   -HeaderRows:list(pair(term,positive_integer))
%! ) is det.

tree_headers([], []).
tree_headers([_-Pairs|_], [HeaderRow]):-
  (   Pairs == []
  ->  HeaderRow = []
  ;   Pairs = [_-Measure|_],
      \+ is_list(Measure)
  ->  maplist(pair_second, Pairs, HeaderRow)
  ;   fail
  ).
tree_headers(Trees, [HeaderRow|HeaderRows]):-
  pairs_keys_values(Trees, Roots, Subtrees),
  maplist(length, Subtrees, NumberOfColumns),
  pairs_keys_values(HeaderRow, Roots, NumberOfColumns),
  append(Subtrees, Trees0),
  tree_headers(Trees0, HeaderRows).

