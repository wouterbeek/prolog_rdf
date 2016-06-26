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
:- use_module(library(semweb/rdf11)).

:- rdf_meta
   rdfh_fca(r, ?, ?),
   rdfh_fca(r, :, ?, ?).





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
