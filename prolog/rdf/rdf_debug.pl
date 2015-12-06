:- module(
  rdf_debug,
  [
    rdf_show_graph/1, % +Graph
    rdf_show_graph/2 % +Graph:rdf_graph
                     % +Options:list(compound)
  ]
).

/** <module> RDF debug tools

Show RDF data structures during modeling/development.

@author Wouter Beek
@version 2015/08, 2015/12
*/

:- use_module(library(gv/gv_file)).
:- use_module(library(option)).
:- use_module(library(os/process_ext)).
:- use_module(library(rdf/rdf_graph_viz)).

:- rdf_meta(rdf_show_graph(r)).
:- rdf_meta(rdf_show_graph(r,+)).

:- predicate_options(rdf_show_graph/2, 2, [
     pass_to(rdf_graph_to_export_graph/3, 3),
     pass_to(gv_export/3, 3),
     pass_to(run_process/3, 3)
   ]).





%! rdf_show_graph(+Graph:rdf_graph) is det.
% Wrapper around rdf_show_graph/2 with default options.

rdf_show_graph(G):-
  rdf_show_graph(G, []).


%! rdf_show_graph(+Graph:rdf_graph, +Options:list(compound)) is det.

rdf_show_graph(G, Opts1):-
  rdf_graph_to_export_graph(G, ExportG, Opts1),
  file_name_extension(G, pdf, File),
  gv_export(ExportG, File, Opts1),
  merge_options([detached(true),program('XPDF')], Opts1, Opts2),
  run_process(xpdf, [file(File)], Opts2).
