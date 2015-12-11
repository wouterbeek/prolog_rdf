:- module(
  rdf_fca_viz,
  [
    rdf_fca_export_graph/2, % +Graph, -ExportGraph
    rdf_fca_export_graph/3, % +Graph:rdf_graph
                            % -ExportGraph:compound
                            % :Options:list(compound)
    rdf_fca_viz/2, % +Graph, +File
    rdf_fca_viz/3 % +Graph:rdf_graph
                  % ?File:atom
                  % :Options:list(compound)
  ]
).

/** <module> RDF FCA vizualization

@author Wouter Beek
@version 2015/10, 2015/12
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(fca/fca_viz)).
:- use_module(library(fca/rdf_fca)).
:- use_module(library(gv/gv_file)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_print_term)).

:- rdf_meta(rdf_fca_export_graph(r,-)).
:- rdf_meta(rdf_fca_export_graph(r,-,:)).
:- rdf_meta(rdf_fca_viz(r,+)).
:- rdf_meta(rdf_fca_viz(r,+,:)).

:- predicate_options(rdf_fca_export_graph/3, 3, [
     pass_to(fca_export_graph/3, 3)
   ]).
:- predicate_options(rdf_fca_viz/3, 3, [
     pass_to(rdf_fca_export_graph/3, 3),
     pass_to(gv_export/3, 3)
   ]).





%! rdf_fca_export_graph(+Graph:rdf_graph, -ExportGraph:compound) is det.

rdf_fca_export_graph(G, ExportG):-
  rdf_fca_export_graph(G, ExportG, []).


%! rdf_fca_export_graph(
%!   +Graph:rdf_graph,
%!   -ExportGraph:compound,
%!   +Options:list(compound)
%! ) is det.

rdf_fca_export_graph(G, ExportG, Opts1):-
  rdf_fca_from_graph(G, Context),
  merge_options(
    [
      attribute_label(rdf_attribute_label),
      concept_label(both),
      graph_label(G),
      object_label(rdf_object_label)
    ],
    Opts1,
    Opts2
  ),
  fca_export_graph(Context, ExportG, Opts2).



%! rdf_fca_viz(+Graph:rdf_graph, +File:atom) is det.
% Wrapper around rdf_fca_viz/3 with default options.

rdf_fca_viz(G, File):-
  rdf_fca_viz(G, File, []).


%! rdf_fca_viz(+Graph:rdf_graph, +File:atom, +Options:list(compound)) is det.

rdf_fca_viz(G, File, Opts):-
  rdf_fca_export_graph(G, ExportG, Opts),
  gv_export(ExportG, File, Opts).





% HELPERS %

rdf_attribute_label(A) --> rdf_print_term(A).

rdf_object_label(A)    --> rdf_print_term(A).
