:- module(
  q_html,
  [
    rdf_html_dataset_graph_menu//3, % +M, +D, +G
    rdf_html_graph_menu//1,         % +M
    rdf_html_graph_menu//2          % +Attrs, +M
  ]
).

/** <module> Quine HTML
*/

:- use_module(library(gis/gis)).

:- rdf_meta
   rdf_html_dataset_graph_menu(+, r, r, ?, ?).





%! rdf_html_dataset_graph_menu(+M, +D, +G)// is det.
%
% @tbd `class(selected)` does not show a selection in the HTML page.
%
% @tbd JavaScript does not work; cannot extract the `value` attribute.

rdf_html_dataset_graph_menu(M, D, G) -->
  {
    rdf_dataset_trees(M, number_of_triples, Trees),
    Trees \== []
  }, !,
  navbar_dropdown_menu(
    'dataset-graph-menu',
    "Graph",
    rdf_html_dataset_graph_menu_item(D, G),
    Trees
  ),
  js_script({|javascript(_)||
$("#dataset-graph-menu").on('change', function(){
  var urls = [];
  $(this).children(':selected').each(function(i,el) {
    urls.push(el.value);
  })
  console.log(urls);
  alert(urls);
});
  |}).
rdf_html_dataset_graph_menu(_, _, _) --> [].

rdf_html_dataset_graph_menu_item(D0, G0, t(D,Trees)) -->
  {with_output_to(string(Lbl), rdf_print_dataset_term(D))},
  html(
    optgroup(label(Lbl),
      \html_maplist(
        rdf_html_graph_menu_item0(D0, D, G0),
        Trees
      )
    )
  ).

rdf_html_graph_menu_item0(D0, D, G0, t(G,[])) -->
  {(D0 = D, G0 = G -> Selected = true ; Selected = false)},
  rdf_html_graph_menu_item(Selected, G).

rdf_html_dataset_term0(D, _) -->
  {q_dataset_label(D, Lbl)}, !,
  html(Lbl).

rdf_html_graph_term0(G, Opts) -->
  {
    q_dataset_graph(D, G),
    q_graph_label(G, Lbl)
  }, !,
  html([\rdf_html_dataset_term0(D, Opts),"/",Lbl]).



%! rdf_html_graph_menu(+M)// is det.
%! rdf_html_graph_menu(+Attrs, +M)// is det.

rdf_html_graph_menu(M) -->
  rdf_html_graph_menu([], M).


rdf_html_graph_menu(Attrs1, M) -->
  {
    % @tbd Special GIS case should be fixed in module `gis_db`.
    (   M == gis
    ->  findall(G, gis_graph(G), Gs)
    ;   findall(NumTriples-G, rdf_number_of_triples(M, G, NumTriples), Pairs),
        desc_pairs_values(Pairs, Gs)
    ),
    Gs \== [],
    merge_attrs(Attrs1, [class='navbar-left'], Attrs2)
  },
  navbar_dropdown_menu(
    Attrs2,
    'graph-menu',
    "Graph",
    rdf_html_graph_menu_item(false),
    Gs
  ).
rdf_html_graph_menu(_, _) --> [].

rdf_html_graph_menu_item(Selected, G) -->
  {(Selected == true -> T = [class(selected)] ; T = [])},
  html(
    option([value(G)|T],
      \rdf_html_graph_term(G, _{handle_id: none})
    )
  ).
