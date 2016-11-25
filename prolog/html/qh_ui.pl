:- module(
  qh_ui,
  [
    qh_dataset_graph_menu//3, % +M, +D, +G
    qh_describe//3,           % +M, +S, +G
    qh_describe//4,           % +M, +S, +G          +Opts
    qh_graph_menu//1,         % +M
    qh_graph_menu//2,         % +Attrs, +M
    qh_graph_table//1,        %                     +Opts
    qh_p_os_table//1,         % +Pairs
    qh_p_os_table//2,         % +Pairs,             +Opts
    qh_quad_panels//5,        % +M, ?S, ?P, ?O, ?G
    qh_quad_panels//6,        % +M, ?S, ?P, ?O, ?G, +Opts
    qh_quad_table//1,         %     +Quads
    qh_quad_table//6,         % +M, ?S, ?P, ?O, ?G, +Opts
    qh_tree//1,               %     +Tree
    qh_tree//2,               %     +Tree,          +Opts
    qh_triple_table//1,       %     +Triples
    qh_triple_table//2,       %     +Triples,       +Opts
    qh_triple_table//5,       % +M, ?S, ?P, ?O, ?G
    qh_triple_table//6        % +M, ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Quine HTML UI components

@author Wouter Beek
@version 2016/07-2016/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(gis/gis)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/qh)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_dataset_api)).
:- use_module(library(q/q_graph)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(string_ext)).
:- use_module(library(tree/s_tree)).
:- use_module(library(yall)).

:- rdf_meta
   qh_dataset_graph_menu(+, r, r, ?, ?),
   qh_describe(+, r, r, ?, ?),
   qh_describe(+, r, r, +, ?, ?),
   qh_quad_panels(+, r, r, o, r, ?, ?),
   qh_quad_panels(+, r, r, o, r, +, ?, ?),
   qh_quad_table(+, r, r, o, r, ?, ?),
   qh_quad_table(+, r, r, o, r, +, ?, ?),
   qh_triple_table(+, r, r, o, r, ?, ?),
   qh_triple_table(+, r, r, o, r, +, ?, ?).





%! qh_dataset_graph_menu(+M, +D, +G)// is det.
%
% @tbd `class(selected)` does not show a selection in the HTML page.
%
% @tbd JavaScript does not work; cannot extract the `value` attribute.

qh_dataset_graph_menu(M, D, G) -->
  {
    q_dataset_trees(M, number_of_triples, Trees),
    Trees \== []
  }, !,
  navbar_dropdown_menu(
    'dataset-graph-menu',
    "Graph",
    qh_dataset_graph_menu_item(D, G),
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
qh_dataset_graph_menu(_, _, _) --> [].


qh_dataset_graph_menu_item(D0, G0, t(D,Trees)) -->
  {with_output_to(string(Lbl), q_print_dataset_term(D))},
  html(
    optgroup(label(Lbl),
      \html_maplist(
        qh_graph_menu_item0(D0, D, G0),
        Trees
      )
    )
  ).


qh_graph_menu_item0(D0, D, G0, t(G,[])) -->
  {(D0 = D, G0 = G -> Selected = true ; Selected = false)},
  qh_graph_menu_item(Selected, G).



%! qh_describe(+M, +S, +G)// is det.
%! qh_describe(+M, +S, +G, +Opts)// is det.
%
% Generate a full description of subject term S.
%
% @tbd Align with (S)CBD.

qh_describe(M, S, G) -->
  qh_describe(M, S, G, _{}).


qh_describe(M, S, G, Opts) -->
  {
    findall(P-O, q(M, S, P, O, G), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  qh_p_os_table(Groups, Opts).



%! qh_graph_menu(+M)// is det.
%! qh_graph_menu(+Attrs, +M)// is det.

qh_graph_menu(M) -->
  qh_graph_menu([], M).


qh_graph_menu(Attrs1, M) -->
  {
    % @tbd Special GIS case should be fixed in module `gis_db`.
    (   M == gis
    ->  findall(G, gis_graph(G), Gs)
    ;   findall(NumTriples-G, q_number_of_triples(M, G, NumTriples), Pairs),
        desc_pairs_values(Pairs, Gs)
    ),
    Gs \== [],
    merge_attrs(Attrs1, [class='navbar-left'], Attrs2)
  },
  navbar_dropdown_menu(
    Attrs2,
    'graph-menu',
    "Graph",
    qh_graph_menu_item(false),
    Gs
  ).
qh_graph_menu(_, _) --> [].


qh_graph_menu_item(Selected, G) -->
  {(Selected == true -> T = [class(selected)] ; T = [])},
  html(
    option([value(G)|T],
      \qh_graph_term(G, _{handle_id: none})
    )
  ).



%! qh_graph_table(+Opts)// is det.

qh_graph_table(Opts1) -->
  {
    q_graph_table_comps(HeaderRow, DataRows),
    qh_default_table_options(Opts1, Opts2)
  },
  table_content(html_hook(Opts2), [head(HeaderRow)|DataRows]).



%! qh_p_os_table(+Groups)// is det.
%! qh_p_os_table(+Groups, +Opts)// is det.

qh_p_os_table(Groups) -->
  qh_p_os_table(Groups, _{}).


qh_p_os_table(Groups, Opts1) -->
  {
    HeaderRow = ["Predicate","Objects"],
    qh_default_table_options(Opts1, Opts2)
  },
  table(
    \table_header_row(HeaderRow),
    \html_maplist(
      {Opts2}/[Group]>>qh_p_os_row0(Group, Opts2),
      Groups
    )
  ).


qh_p_os_row0(Opts, P-Os) -->
  html(
    tr([
      td(\qh_predicate(P, Opts)),
      td(
	\html_seplist(
	  {Opts}/[O]>>qh_object(O, Opts),
	  " ",
	  Os
	)
      )
    ])
  ).



%! qh_quad_panels(+M, ?S, ?P, ?O, ?G)// is det.
%! qh_quad_panels(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

qh_quad_panels(M, S, P, O, G) -->
  qh_quad_panels(M, S, P, O, G, _{}).


qh_quad_panels(M, S, P, O, G, Opts1) -->
  {
    qh_default_table_options(Opts1, Opts2),
    findall(G-Triple, q_triple(M, S, P, O, G, Triple), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  panels(
    {Opts2}/[Group]>>qh_triple_table(Group, Opts2),
    Groups
  ).



%! qh_quad_table(+Quads)// is det.
%! qh_quad_table(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

qh_quad_table(Quads) -->
  {
    HeaderRow = ["Subject","Predicate","Object","Graph"],
    qh_default_table_options(Opts)
  },
  table(
    \table_header_row(HeaderRow),
    \html_maplist({Opts}/[Quad]>>qh_quad_row0(Quad, Opts), Quads)
  ).


qh_quad_table(M, S, P, O, G, Opts1) -->
  {
    qh_default_table_options(Opts1, Opts2),
    q_quads(M, S, P, O, G, Quads)
  },
  qh_quad_table(Quads, Opts2).


qh_quad_row0(rdf(S,P,O,G), Opts) -->
  html(
    tr([
      td(class='col-md-3',
        \internal_link(
	  link_to_id(subject_handler,[subject(S)]),
          \qh_subject(S, Opts)
        )
      ),
      td(class='col-md-3',
        \internal_link(
          link_to_id(predicate_handler,[predicate(P)]),
          \qh_predicate(P, Opts)
        )
      ),
      td(class='col-md-3',
        \internal_link(
          link_to_id(object_handler,[object(O)]),
          \qh_object(O, Opts)
        )
      ),
      td(class='col-md-3', \graph_link(G))
    ])
  ).



%! qh_tree(+Tree)// is det.
%! qh_tree(+Tree, +Opts)// is det.

qh_tree(Tree) -->
  qh_tree(Tree, _{}).


qh_tree(Tree, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  html([
    \check_all0,
    div(class(treeview), div(class(tree),\qh_trees0([0], [Tree], Opts2)))
  ]).


check_all0 -->
  html([
    \js_script({|javascript(_)||
$("#checkAll").change(function () {
  $("input:checkbox").prop('checked', $(this).prop("checked"));
});
    |}),
    p(label([input([id=checkAll,type=checkbox], []), "Check all"]))
  ]).


qh_trees0(_, [], _) --> !, [].
qh_trees0(Ns, [P-[Leaf-[]]|Trees], Opts) --> !,
  html([
    div(class=node, [
      \qh_predicate(P, Opts),
      " ",
      \qh_object(Leaf, Opts)
    ]),
    \qh_trees0(Ns, Trees, Opts)
  ]).
qh_trees0(Ns, [Leaf-[]|Trees], Opts) --> !,
  html([
    div(class=node, \qh_object(Leaf, Opts)),
    \qh_trees0(Ns, Trees, Opts)
  ]).
qh_trees0(Ns1, [Root-Subtrees|Trees], Opts) -->
  {
    atomic_list_concat([item|Ns1], -, Id),
    append(Ns, [N1], Ns1),
    N2 is N1 + 1,
    append(Ns, [N2], Ns2),
    append(Ns1, [0], Ns3)
  },
  html([
    div(class=node, [
      input([id=Id,type=checkbox], []),
      label(for=Id, \qh_predicate(Root, Opts)),
      div(class=tree, \qh_trees0(Ns3, Subtrees, Opts))
    ]),
    \qh_trees0(Ns2, Trees, Opts)
  ]).



%! qh_triple_table(+Triples)// is det.
%! qh_triple_table(+Triples, +Opts)// is det.

qh_triple_table(Triples) -->
  qh_triple_table(Triples, _{}).


qh_triple_table(Triples, Opts1) -->
  {
    HeaderRow = ["Subject","Predicate","Object"],
    qh_default_table_options(Opts1, Opts2)
  },
  table(
    \table_header_row(HeaderRow),
    \html_maplist(
      {Opts2}/[Triple]>>qh_triple_row0(Triple, Opts2),
      Triples
    )
  ).


qh_triple_row0(rdf(S,P,O), Opts) -->
  html(
    tr(class=triple, [
      td(class='col-md-4',
        \qh_subject(S, Opts.put(_{query_key: subject}))
      ),
      td(class='col-md-3',
        \qh_predicate(P, Opts.put(_{query_key: predicate}))
      ),
      td(class='col-md-5',
        \qh_object(O, Opts.put(_{query_key: object}))
      )
    ])
  ).



%! qh_triple_table(+M, ?S, ?P, ?O, ?G)// is det.
%! qh_triple_table(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

qh_triple_table(M, S, P, O, G) -->
  qh_triple_table(M, S, P, O, G, _{}).


qh_triple_table(M, S, P, O, G, Opts1) -->
  {
    qh_default_table_options(Opts1, Opts2),
    q_triples(M, S, P, O, G, Triples)
  },
  qh_triple_table(Triples, Opts2).





% HELPERS %

%! qh_default_table_options(-Opts) is det.
%! qh_default_table_options(+Opts1, -Opts2) is det.

qh_default_table_options(Opts) :-
  qh_default_table_options(_{}, Opts).


qh_default_table_options(Opts1, Opts2) :-
  qh:qh_default_options(_{max_iri_length: 25}, DefOpts),
  merge_dicts(DefOpts, Opts1, Opts2).
