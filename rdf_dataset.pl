:- module(
  rdf_dataset,
  [
    rdf_assert_dataset/1, % +RdfDataset:compound
    rdf_dataset/1, % ?RdfDataset:compound
    rdf_dataset/3, % ?DefaultGraph:atom
                   % ?NamedGraphs:list(iri)
                   % ?RdfDataset:compound
    rdf_default_graph/2, % ?RdfDataset:compound
                         % ?DefaultGraph:atom
    rdf_graph/2, % ?RdfDataset:compound
                 % ?Graph:atom
    rdf_named_graph/2 % ?RdfDataset:compound
                      % ?NamedGraph:iri
  ]
).

/** <module> RDF dataset

@author Wouter Beek
@tbd rdf_dataset_merge(+Dataset1:compound, +Dataset2:compound, -MergedDataset:compound) is det.
@version 2013/09-2013/10, 2014/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)).
:- use_module(void(void_db)). % XML namespace.



%! rdf_assert_dataset(+DefaultGraph:atom) is det.

rdf_assert_dataset(RdfDataset):-
  rdf_dataset(RdfDataset, DefaultGraph, NamedGraphs),
  maplist(rdf_assert_named_graph(DefaultGraph), NamedGraphs).

rdf_assert_named_graph(DefaultGraph, NamedGraph):-
  rdf_assert_individual(NamedGraph, void:'Dataset', DefaultGraph).


%! rdf_dataset(+RdfDataset:compound) is semidet.
%! rdf_dataset(-RdfDataset:compound) is nondet.

rdf_dataset(RdfDataset):-
  rdf([graph_mode(no_index)], _, rdf:type, void:'Dataset', DefaultGraph),
  aggregate_all(
    set(NamedGraph),
    rdf(
      [graph_mode(no_index)],
      NamedGraph,
      rdf:type,
      void:'Dataset',
      DefaultGraph
    ),
    NamedGraphs
  ),
  rdf_dataset(DefaultGraph, NamedGraphs, RdfDataset).


%! rdf_dataset(
%!   +DefaultGraph:atom,
%!   +NamedGraphs:list(pair(iri,atom)),
%!   +RdfDataset:compound
%! ) is semidet.
%! rdf_dataset(
%!   +DefaultGraph:atom,
%!   +NamedGraphs:list(pair(iri,atom)),
%!   -RdfDataset:compound
%! ) is det.
%! rdf_dataset(
%!   -DefaultGraph:atom,
%!   -NamedGraphs:list(pair(iri,atom)),
%!   +RdfDataset:compound
%! ) is det.

rdf_dataset(DefaultGraph, NamedGraphs, rdf_dataset(DefaultGraph,NamedGraphs)).


%! rdf_default_graph(+RdfDataset:compound, +DefaultGraph:atom) is semidet.
%! rdf_default_graph(+RdfDataset:compound, -DefaultGraph:atom) is det.

rdf_default_graph(rdf_dataset(DefaultGraph,_), DefaultGraph).


%! rdf_graph(+RdfDataset:compound, +Graph:atom) is semidet.
%! rdf_graph(+RdfDataset:compound, -Graph:atom) is nondet.

rdf_graph(RdfDataset, DefaultGraph):-
  rdf_default_graph(RdfDataset, DefaultGraph).
rdf_graph(RdfDataset, NamedGraph):-
  rdf_named_graph(RdfDataset, NamedGraph).


%! rdf_named_graph(+RdfDataset:compound, +NamedGraph:atom) is semidet.
%! rdf_named_graph(+RdfDataset:compound, -NamedGraph:atom) is nondet.

rdf_named_graph(rdf_dataset(_,NamedGraphs), NamedGraph):-
  member(NamedGraph, NamedGraphs).

