:- module(
  rdf_debug,
  [
    rdf_show_graph/1, % +G
    rdf_show_graph/2  % +G, +Opts
  ]
).

/** <module> RDF debug tools

Show RDF data structures during modeling/development.

@author Wouter Beek
@version 2015/08, 2015/12-2016/01
*/

:- use_module(library(gv/gv_file)).
:- use_module(library(option)).
:- use_module(library(os/process_ext)).
:- use_module(library(rdf/rdf_graph_viz)).

:- rdf_meta(rdf_show_graph(r)).
:- rdf_meta(rdf_show_graph(r,+)).

:- predicate_options(rdf_show_graph/2, 2, [
     pass_to(rdf_graph_to_export_graph/3, 3),
     pass_to(graph_viz/3, 3),
     pass_to(run_process/3, 3)
   ]).



%! rdf_show_graph(+G) is det.
%! rdf_show_graph(+G, +Opts) is det.

rdf_show_graph(G) :-
  rdf_show_graph(G, []).
rdf_show_graph(G, Opts1) :-
  rdf_graph_to_export_graph(G, ExportG, Opts1),
  file_name_extension(G, pdf, File),
  graph_viz(ExportG, File, Opts1),
  merge_options([detached(true),program('XPDF')], Opts1, Opts2),
  run_process(xpdf, [file(File)], Opts2).
