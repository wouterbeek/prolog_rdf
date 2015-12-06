:- module(
  rdf_html_fca_viz,
  [
    rdf_html_fca_viz//1, % +Graph
    rdf_html_fca_viz//2 % +Graph:rdf_graph
                        % +Options:list(compound)
  ]
).

/** <module> RDF HTML FCA visualization

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(fca/rdf_fca_viz)).
:- use_module(library(gv/gv_dom)).
:- use_module(library(html/html_dom)).

:- rdf_meta(rdf_html_fca_viz(r,?,?)).
:- rdf_meta(rdf_html_fca_viz(r,:,?,?)).

:- predicate_options(rdf_html_fca_viz//2, 2, [
     pass_to(rdf_graph_to_export_graph/3, 3)
   ]).





%! rdf_html_fca_viz(+Graph:rdf_graph)// is det.
% Wrapper around rdf_html_fca_viz//2 with default options.

rdf_html_fca_viz(G) -->
  rdf_html_fca_viz(G, []).


%! rdf_html_fca_viz(+Graph:rdf_graph, +Options:list(compound))// is det.

rdf_html_fca_viz(G, Opts) -->
  {
    rdf_fca_export_graph(G, ExportG, Opts),
    gv_dom(ExportG, Dom, [method(dot)])
  },
  html_insert_dom(Dom).
