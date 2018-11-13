:- module(
  rdfs_fca_viz,
  [
    rdfs_fca_export_graph/2, % +Backend, -ExportBackend
    rdfs_fca_export_graph/3, % +Backend, -ExportBackend, +Options
    rdfs_fca_viz/2,          % +Backend, +File
    rdfs_fca_viz/3           % +Backend, ?File, +Options
  ]
).

/** <module> Vizualization for FCA for RDFS

@author Wouter Beek
@version 2015/10, 2015/12-2016/01, 2016/03, 2016/06, 2016/12
*/

:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(fca/fca_viz)).
:- use_module(library(fca/rdfs_fca)).
:- use_module(library(gv/gv_file)).
:- use_module(library(rdf/rdf_print)).

:- rdf_meta
   rdfs_fca_export_graph(r, -),
   rdfs_fca_export_graph(r, -, :),
   rdfs_fca_viz(r, +),
   rdfs_fca_viz(r, +, :).





%! rdfs_fca_export_graph(+Backend, -ExportBackend) is det.
%! rdfs_fca_export_graph(+Backend, -ExportBackend, Options:dict) is det.

rdfs_fca_export_graph(B, ExportB) :-
  rdfs_fca_export_graph(B, ExportB, []).


rdfs_fca_export_graph(B, ExportB, Options1) :-
  rdfs_fca_context(B, Context),
  merge_dicts(
    options{
      attribute_label: rdf_dcg_node,
      concept_label: both,
      object_label: rdf_dcg_node
    },
    Options1,
    Options2
  ),
  fca_export_graph(Context, ExportB, Options2).



%! rdfs_fca_viz(+Backend, +File:atom) is det.
%! rdfs_fca_viz(+Backend, +File:atom, +Options:dict) is det.

rdfs_fca_viz(B, File) :-
  rdfs_fca_viz(B, File, options{}).


rdfs_fca_viz(B, File, Options) :-
  rdfs_fca_export_graph(B, ExportB, Options),
  graph_viz(ExportB, File, Options).
