:- module(
  qh_ui,
  [
    qh_describe//2,      % +M, +S
    qh_describe//3,      % +M, +S,             +Opts
    qh_graph_menu//1,    % +M
    qh_graph_table//0,
    qh_graph_table//1,   %                     +Opts
    qh_quad_panels//5,   % +M, ?S, ?P, ?O, ?G
    qh_quad_panels//6,   % +M, ?S, ?P, ?O, ?G, +Opts
    qh_quad_table//1,    % +Quads
    qh_quad_table//2,    % +M, +Quads
    qh_quad_table//3,    % +M, +Quads,         +Opts
    qh_quad_table//5,    % +M, ?S, ?P, ?O, ?G
    qh_quad_table//6,    % +M, ?S, ?P, ?O, ?G, +Opts
    qh_tree//2,          % +M, +Tree
    qh_tree//3,          % +M, +Tree,          +Opts
    qh_triple_table//1,  % +Triples
    qh_triple_table//2,  % +M, +Triples
    qh_triple_table//3,  % +M, +Triples,       +Opts
    qh_triple_table//5,  % +M, ?S, ?P, ?O, ?G
    qh_triple_table//6   % +M, ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Quine HTML UI components

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(aggregate)).
:- use_module(library(dict_ext)).
:- use_module(library(gis/gis_db)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/qh)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(q/q_stmt)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- rdf_meta
   qh_describe(+, r, ?, ?),
   qh_describe(+, r, +, ?, ?),
   qh_quad_panels(+, r, r, o, r, ?, ?),
   qh_quad_panels(+, r, r, o, r, +, ?, ?),
   qh_quad_table(+, r, r, o, r, ?, ?),
   qh_quad_table(+, r, r, o, r, +, ?, ?),
   qh_triple_table(+, r, r, o, r, ?, ?),
   qh_triple_table(+, r, r, o, r, +, ?, ?).





%! qh_describe(+M, +S)// is det.
%! qh_describe(+M, +S, +Opts)// is det.
%
% Generate a full description of subject term S.

qh_describe(M, S) -->
  qh_describe(M, S, _{}).


qh_describe(M, S, Opts1) -->
  {
    qh_default_table_options(Opts1, Opts2),
    findall(P-O, q(M, S, P, O), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  bs_table(
    \bs_table_header(["predicate","objects"]),
    \html_maplist(qh_describe_row0(M, Opts2), Groups)
  ).


qh_describe_row0(M, Opts, P-Os) -->
  html(
    tr([
      td(\qh_property_outer0(M, property, [property], Opts, P)),
      td(\html_seplist(qh_object_outer0(M, object, [object], Opts), " ", Os))
    ])
  ).



%! qh_graph_menu(+M)// is det.

qh_graph_menu(M) -->
  {
    findall(
      N-G,
      (
        gis_graph(M, G),
        q_number_of_triples(M, G, N)
      ),
      Pairs
    ),
    Pairs \== [], !,
    desc_pairs_values(Pairs, Gs)
  },
  html(
    form([class=['navbar-form','navbar-right'],role=search],
      div(class='form-group', [
        label(for='graph-menu', "Graph: "),
        select([class=['form-control',selectpicker],id='graph-menu'],
          \html_maplist(qh_graph_menu_item(M), Gs)
        )
      ])
    )
  ).
qh_graph_menu(_) --> [].


qh_graph_menu_item(M, G) -->
  {term_to_atom(G, Val)},
  html(
    option(value=Val,
      \qh_graph_term(M, G, _{qh_link: false})
    )
  ).



%! qh_graph_table// is det.
%! qh_graph_table(+Opts)// is det.

qh_graph_table -->
  qh_graph_table(_{}).


qh_graph_table(Opts1) -->
  {
    qh_default_options(Opts1, Opts2),
    findall(N-[M,G], q_number_of_triples(M, G, N), Pairs),
    desc_pairs_values(Pairs, Vals)
  },
  bs_table(
    \bs_table_header(["graph","№ triples","store"]),
    \html_maplist(qh_graph_row0(Opts2), Vals)
  ).


qh_graph_row0(Opts, [M,G]) -->
  {q_number_of_triples(M, G, N)},
  html(
    tr([
      td(\qh_graph_term_outer0(M, graph, [graph], Opts, G)),
      td(\html_thousands(N)),
      td(M)
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
  bs_panels({M,Opts2}/[Group]>>qh_triple_table(M, Group, Opts2), Groups).



%! qh_quad_table(+Quads)// is det.
%! qh_quad_table(+M, +Quads)// is det.
%! qh_quad_table(+M, +Quads, +Opts)// is det.
%! qh_quad_table(+M, ?S, ?P, ?O, ?G)// is det.
%! qh_quad_table(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

qh_quad_table(Quads) -->
  qh_quad_table(_, Quads).


qh_quad_table(M, Quads) -->
  qh_quad_table(M, Quads, _{}).


qh_quad_table(M, Quads, Opts1) -->
  {qh_default_table_options(Opts1, Opts2)},
  bs_table(
    \bs_table_header(["Subject","Predicate","Object","Graph"]),
    \html_maplist(qh_quad_row0(M, Opts2), Quads)
  ).


qh_quad_table(M, S, P, O, G) -->
  qh_quad_table(M, S, P, O, G, _{}).


qh_quad_table(M, S, P, O, G, Opts1) -->
  {
    qh_default_table_options(Opts1, Opts2),
    q_quads(M, S, P, O, G, Quads)
  },
  qh_quad_table(M, Quads, Opts2).



qh_quad_row0(M, Opts1, rdf(S,P,O,G)) -->
  {
    (del_dict(query, Opts1, Query1, Opts2) -> true ; Opts2 = Opts1),
    qh_link_query_term(graph, G, QueryTerm),
    union(Query1, [QueryTerm], Query2),
    put_dict(query, Opts2, Query2, Opts3)
  },
  html(
    span(class=quadruple,
      tr([
        td(\qh_subject_outer0(M, term, [subject], Opts3, S)),
        td(\qh_predicate_outer0(M, predicate, [predicate], Opts3, P)),
        td(\qh_object_outer0(M, term, [object], Opts3, O)),
        td(\qh_graph_term_outer0(M, graph, [graph], Opts1, G))
      ])
    )
  ).



%! qh_tree(+M, +Tree)// is det.
%! qh_tree(+M, +Tree, +Opts)// is det.

qh_tree(M, Tree) -->
  qh_tree(M, Tree, _{}).


qh_tree(M, Tree, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  html([
    \check_all0,
    div(class(treeview), div(class(tree),\qh_trees0(M, [0], [Tree], Opts2)))
  ]).


check_all0 -->
  html([
    \js_script({|javascript(_)||
$("#checkAll").change(function () {
  $("input:checkbox").prop('checked', $(this).prop("checked"));
});
    |}),
    p(label([input([id(checkAll),type(checkbox)], []), "Check all"]))
  ]).


qh_trees0(_, _, [], _) --> !, [].
qh_trees0(M, Ns, [P-[Leaf-[]]|Trees], Opts) --> !,
  html([
    div(class=node, [
      \qh_predicate_outer0(M, predicate, [predicate], Opts, P),
      " ",
      \qh_object_outer0(M, object, [object], Opts, Leaf)
    ]),
    \qh_trees0(M, Ns, Trees, Opts)
  ]).
qh_trees0(M, Ns, [Leaf-[]|Trees], Opts) --> !,
  html([
    div(class=node, \qh_object_outer0(M, object, [object], Opts, Leaf)),
    \qh_trees0(M, Ns, Trees, Opts)
  ]).
qh_trees0(M, Ns1, [Root-Subtrees|Trees], Opts) -->
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
      label(for=Id,
        \qh_predicate_outer0(M, predicate, [predicate], Opts, Root)
      ),
      div(class=tree, \qh_trees0(M, Ns3, Subtrees, Opts))
    ]),
    \qh_trees0(M, Ns2, Trees, Opts)
  ]).



%! qh_triple_table(+Triples)// is det.
%! qh_triple_table(+M, +Triples)// is det.
%! qh_triple_table(+M, +Triples, +Opts)// is det.

qh_triple_table(Triples) -->
  qh_triple_table(_, Triples).


qh_triple_table(M, Triples) -->
  qh_triple_table(M, Triples, _{}).


qh_triple_table(M, Triples, Opts1) -->
  {qh_default_table_options(Opts1, Opts2)},
  bs_table(
    \qh_table_header0,
    \html_maplist(qh_triple_row0(M, Opts2), Triples)
  ).


qh_table_header0 -->
  html(
    tr([
      th(class=subject, "Subject"),
      th(class=predicate, "Predicate"),
      th(class=object, "Object")
    ])
  ).


qh_triple_row0(M, Opts, rdf(S,P,O)) -->
  html(
    span(class=triple,
      tr([
        td(\qh_subject_outer0(M, term, [subject], Opts, S)),
        td(\qh_predicate_outer0(M, predicate, [predicate], Opts, P)),
        td(\qh_object_outer0(M, term, [object], Opts, O))
      ])
    )
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
  qh_triple_table(M, Triples, Opts2).





% HELPERS %

%! qh_default_table_options(+Opts1, -Opts2) is det.

qh_default_table_options(Opts1, Opts2) :-
  qh:qh_default_options(_{}, DefOpts1),
  DefOpts2 = _{max_iri_length: 25, qh_link: true},
  merge_dicts(DefOpts1, DefOpts2, DefOpts),
  merge_dicts(Opts1, DefOpts, Opts2).
