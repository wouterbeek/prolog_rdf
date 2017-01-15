:- module(
  rdf_html_fca,
  [
    rdf_html_fca//1, % +G
    rdf_html_fca//2  % +G, +Opts
  ]
).

/** <module> RDF HTML FCA visualization

@author Wouter Beek
@version 2015/12, 2016/03
*/

:- use_module(library(fca/rdfs_fca_viz)).
:- use_module(library(gv/gv_dom)).
:- use_module(library(html/html_dom)).
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdf_html_fca(r, ?, ?),
   rdf_html_fca(r, :, ?, ?).





%! rdf_html_fca(+G)// is det.
%! rdf_html_fca(+G, +Opts)// is det.

rdf_html_fca(G) -->
  rdf_html_fca(G, []).


rdf_html_fca(G, Opts) -->
  {
    rdfs_fca_export_graph(G, ExportG, Opts),
    gv_dom(ExportG, Dom, [method(dot)])
  },
  html_insert_dom(Dom).
