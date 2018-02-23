:- module(
  rdfs_fca_viz,
  [
    rdfs_fca_export_graph/2, % +G, -ExportG
    rdfs_fca_export_graph/3, % +G, -ExportG, :Opts
    rdfs_fca_viz/2,          % +G, +File
    rdfs_fca_viz/3           % +G, ?File, :Opts
  ]
).

/** <module> Vizualization for FCA for RDFS

@author Wouter Beek
@version 2015/10, 2015/12-2016/01, 2016/03, 2016/06, 2016/12
*/

:- use_module(library(dcg)).
:- use_module(library(fca/fca_viz)).
:- use_module(library(fca/rdfs_fca)).
:- use_module(library(gv/gv_file)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_print)).

:- rdf_meta
   rdfs_fca_export_graph(r, -),
   rdfs_fca_export_graph(r, -, :),
   rdfs_fca_viz(r, +),
   rdfs_fca_viz(r, +, :).





%! rdfs_fca_export_graph(+G, -ExportG) is det.
%! rdfs_fca_export_graph(+G, -ExportG, :Opts) is det.

rdfs_fca_export_graph(G, ExportG) :-
  rdfs_fca_export_graph(G, ExportG, []).


rdfs_fca_export_graph(G, ExportG, Opts1) :-
  rdfs_fca_context(G, Context),
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



%! rdfs_fca_viz(+G, +File) is det.
%! rdfs_fca_viz(+G, +File, +Opts) is det.

rdfs_fca_viz(G, File) :-
  rdfs_fca_viz(G, File, []).


rdfs_fca_viz(G, File, Opts) :-
  rdfs_fca_export_graph(G, ExportG, Opts),
  graph_viz(ExportG, File, Opts).





% HELPERS %

rdf_attribute_label(Term) -->
  dcg_rdf_print_term(Term).



rdf_object_label(Term) -->
  dcg_rdf_print_term(Term).
