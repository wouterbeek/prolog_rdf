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
@version 2015/08, 2015/12-2016/03
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(gv/gv_file)).
:- use_module(library(http/json)).
:- use_module(library(option)).
:- use_module(library(os/process_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(pl_ext)).
:- use_module(library(print_ext)).
:- use_module(library(q/q_graph_viz)).
:- use_module(library(stream_ext)).

:- rdf_meta
   rdf_show_graph(r),
   rdf_show_graph(r, +).





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
