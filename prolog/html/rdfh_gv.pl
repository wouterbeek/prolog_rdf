:- module(
  rdfh_gv,
  [
    rdfh_gv//1, % +G
    rdfh_gv//2  % +G, +Opts
  ]
).

/** <module> RDF HTML graph visualization

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(gv/gv_dom)).
:- use_module(library(html/html_dom)).
:- use_module(library(rdf/rdf_graph_viz)).

:- predicate_options(rdfh_gv//2, 2, [
     pass_to(rdf_graph_to_export_graph/3, 3)
   ]).





%! rdfh_gv(+G)// is det.
%! rdfh_gv(+G, +Opts)// is det.

rdfh_gv(G) -->
  rdfh_gv(G, []).

rdfh_gv(G, Opts) -->
  {
    rdf_graph_to_export_graph(G, ExportG, Opts),
    gv_dom(ExportG, Dom, [method(dot)])
  },
  html_insert_dom(Dom).
