:- module(
  rdf_html_gv,
  [
    rdf_html_gv//1, % +G
    rdf_html_gv//2  % +G, +Opts
  ]
).

/** <module> RDF HTML graph visualization

@author Wouter Beek
@version 2015/12, 2016/12
*/

:- use_module(library(graph/build_export_graph)).
:- use_module(library(gv/gv_dom)).
:- use_module(library(html/html_dom)).
:- use_module(library(q/q_graph_viz)).





%! rdf_html_gv(+G)// is det.
%! rdf_html_gv(+G, +Opts)// is det.

rdf_html_gv(G) -->
  rdf_html_gv(G, []).


rdf_html_gv(G, Opts) -->
  {
    build_export_graph(G, ExportG, Opts),
    gv_dom(ExportG, Dom, [method(dot)])
  },
  html_insert_dom(Dom).
