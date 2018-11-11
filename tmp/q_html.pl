:- module(
  q_html,
  [
    rdf_html_dataset_graph_menu//3, % +Backend, +D, +G
    rdf_html_graph_menu//1,         % +Backend
    rdf_html_graph_menu//2          % +Attrs, +Backend
  ]
).

/** <module> Quine HTML
*/

:- use_module(library(gis/gis_api)).

:- rdf_meta
   rdf_html_dataset_graph_menu(+, r, r, ?, ?).





%! rdf_html_dataset_graph_menu(+Backend, +D, +G)// is det.
%
% @tbd `class(selected)` does not show a selection in the HTML page.
%
% @tbd JavaScript does not work; cannot extract the `value` attribute.

rdf_html_dataset_graph_menu(B, D, G) -->
  {
    rdf_dataset_trees(B, number_of_triples, Trees),
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
  {string_phrase(rdf_dcg_term(D), Label)},
  html(
    optgroup(label(Label),
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
  {q_dataset_label(D, Label)}, !,
  html(Label).

rdf_html_graph_term0(G, Opts) -->
  {
    q_dataset_graph(D, G),
    q_graph_label(G, Label)
  }, !,
  html([\rdf_html_dataset_term0(D, Opts),"/",Label]).



%! rdf_html_graph_menu(+Backend)// is det.
%! rdf_html_graph_menu(+Backend, +Attributes)// is det.

rdf_html_graph_menu(B) -->
  rdf_html_graph_menu(B, []).


rdf_html_graph_menu(B, Attrs1) -->
  {
    findall(
      G,
      (
        gis_graph(B, G),
        rdf_number_of_triples(B, G, NumTriples)
      ),
      Pairs
    ),
    desc_pairs_values(Pairs, Gs),
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
