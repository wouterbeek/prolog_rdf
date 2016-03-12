:- module(
  rdfh_fca,
  [
    rdfh_fca//1, % +G
    rdfh_fca//2  % +G, +Opts
  ]
).

/** <module> RDF HTML FCA visualization

@author Wouter Beek
@version 2015/12, 2016/03
*/

:- use_module(library(fca/rdf_fca_viz)).
:- use_module(library(gv/gv_dom)).
:- use_module(library(html/html_dom)).
:- use_module(library(rdf/rdf_ext)).

:- rdf_meta
   rdfh_fca(r, ?, ?),
   rdfh_fca(r, :, ?, ?).

:- predicate_options(rdfh_fca//2, 2, [
     pass_to(rdf_graph_to_export_graph/3, 3)
   ]).





%! rdfh_fca(+G)// is det.
%! rdfh_fca(+G, +Opts)// is det.

rdfh_fca(G) -->
  rdfh_fca(G, []).

rdfh_fca(G, Opts) -->
  {
    rdf_fca_export_graph(G, ExportG, Opts),
    gv_dom(ExportG, Dom, [method(dot)])
  },
  html_insert_dom(Dom).
