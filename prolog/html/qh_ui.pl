:- module(
  qh_ui,
  [
    qh_dataset_table//0,
    qh_dataset_table//1, %                     +Opts
    qh_describe//2,      % +M, +S
    qh_describe//3,      % +M, +S,             +Opts
    qh_graph_menu//1,    % +M
    qh_graph_table//0,
    qh_graph_table//1,   %                     +Opts
    qh_quad_panels//5,   % +M, ?S, ?P, ?O, ?G
    qh_quad_panels//6,   % +M, ?S, ?P, ?O, ?G, +Opts
    qh_quad_table//1,    %     +Quads
    qh_quad_table//2,    %     +Quads,         +Opts
    qh_quad_table//5,    % +M, ?S, ?P, ?O, ?G
    qh_quad_table//6,    % +M, ?S, ?P, ?O, ?G, +Opts
    qh_tree//1,          %     +Tree
    qh_tree//2,          %     +Tree,          +Opts
    qh_triple_table//1,  %     +Triples
    qh_triple_table//2,  %     +Triples,       +Opts
    qh_triple_table//5,  % +M, ?S, ?P, ?O, ?G
    qh_triple_table//6   % +M, ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Quine HTML UI components

@author Wouter Beek
@version 2016/07-2016/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(gis/gis_db)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_ext)).
:- use_module(library(html/html_pl)).
:- use_module(library(html/qh)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_io)).
:- use_module(library(q/q_print)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(tree/s_tree)).
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





%! qh_dataset_table// is det.
%! qh_dataset_table(+Opts)// is det.

qh_dataset_table -->
  qh_dataset_table(_{}).


qh_dataset_table(Opts1) -->
  {
    qh_default_table_options(Opts1, Opts2),
    aggregate_all(set(D), q_loaded_dataset(D), Ds),
    maplist(q_dataset_tree, Ds, SumTriples, Trees),
    pairs_keys_values(Pairs, SumTriples, Trees),
    desc_pairs_values(Pairs, OrderedTrees)
  },
  bs_table(
    \html_table_header_row(["dataset","graph","№ triples","store"]),
    \html_table_trees({Opts2}/[Term]>>qh_something(Term, Opts2), OrderedTrees)
  ).



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
    \html_table_header_row(["predicate","objects"]),
    \html_maplist(qh_describe_row0(Opts2), Groups)
  ).


qh_describe_row0(Opts, P-Os) -->
  html(
    tr([
      td(\qh_property_outer0(property, [property], Opts, P)),
      td(\html_seplist(qh_object_outer0(object, [object], Opts), " ", Os))
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
          \html_maplist(qh_graph_menu_item, Gs)
        )
      ])
    )
  ).
qh_graph_menu(_) --> [].


qh_graph_menu_item(G) -->
  {term_to_atom(G, Val)},
  html(
    option(value=Val,
      \qh_graph_term(G, _{qh_link: false})
    )
  ).



%! qh_graph_table// is det.
%! qh_graph_table(+Opts)// is det.

qh_graph_table -->
  qh_graph_table(_{}).


qh_graph_table(Opts1) -->
  {
    qh_default_table_options(Opts1, Opts2),
    findall(
      NumTriples-[G,NumTriples,M],
      q_number_of_triples(M, G, NumTriples),
      Pairs
    ),
    desc_pairs_values(Pairs, DataRows)
  },
  bs_table(
    \html_table_header_row(["graph","№ triples","store"]),
    \html_maplist(qh_graph_row0(Opts2), DataRows)
  ).


qh_graph_row0(Opts, Row) -->
  html(tr(\qh_graph_cells0(Opts, Row))).


qh_graph_cells0(Opts, [G,NumTriples,Ms]) -->
  html([
    td(\qh_graph_term_outer0(graph, [graph], Opts, G)),
    td(\html_thousands(NumTriples)),
    td(\html_set(Ms))
  ]).



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
  bs_panels({Opts2}/[Group]>>qh_triple_table(Group, Opts2), Groups).



%! qh_quad_table(+Quads)// is det.
%! qh_quad_table(+Quads, +Opts)// is det.
%! qh_quad_table(+M, ?S, ?P, ?O, ?G)// is det.
%! qh_quad_table(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

qh_quad_table(Quads) -->
  qh_quad_table(Quads, _{}).


qh_quad_table(Quads, Opts1) -->
  {qh_default_table_options(Opts1, Opts2)},
  bs_table(
    \html_table_header_row(["subject","predicate","object","graph"]),
    \html_maplist(qh_quad_row0(Opts2), Quads)
  ).


qh_quad_table(M, S, P, O, G) -->
  qh_quad_table(M, S, P, O, G, _{}).


qh_quad_table(M, S, P, O, G, Opts1) -->
  {
    qh_default_table_options(Opts1, Opts2),
    q_quads(M, S, P, O, G, Quads)
  },
  qh_quad_table(Quads, Opts2).



qh_quad_row0(Opts1, rdf(S,P,O,G)) -->
  {
    (del_dict(query, Opts1, Query1, Opts2) -> true ; Opts2 = Opts1),
    qh_link_query_term0(graph, G, QueryTerm),
    union(Query1, [QueryTerm], Query2),
    put_dict(query, Opts2, Query2, Opts3)
  },
  html(
    span(class=quadruple,
      tr([
        td(\qh_subject_outer0(term, [subject], Opts3, S)),
        td(\qh_predicate_outer0(predicate, [predicate], Opts3, P)),
        td(\qh_object_outer0(term, [object], Opts3, O)),
        td(\qh_graph_term_outer0(graph, [graph], Opts1, G))
      ])
    )
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
    p(label([input([id(checkAll),type(checkbox)], []), "Check all"]))
  ]).


qh_trees0(_, [], _) --> !, [].
qh_trees0(Ns, [P-[Leaf-[]]|Trees], Opts) --> !,
  html([
    div(class=node, [
      \qh_predicate_outer0(predicate, [predicate], Opts, P),
      " ",
      \qh_object_outer0(object, [object], Opts, Leaf)
    ]),
    \qh_trees0(Ns, Trees, Opts)
  ]).
qh_trees0(Ns, [Leaf-[]|Trees], Opts) --> !,
  html([
    div(class=node, \qh_object_outer0(object, [object], Opts, Leaf)),
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
      label(for=Id,
        \qh_predicate_outer0(predicate, [predicate], Opts, Root)
      ),
      div(class=tree, \qh_trees0(Ns3, Subtrees, Opts))
    ]),
    \qh_trees0(Ns2, Trees, Opts)
  ]).



%! qh_triple_table(+Triples)// is det.
%! qh_triple_table(+Triples, +Opts)// is det.

qh_triple_table(Triples) -->
  qh_triple_table(Triples, _{}).


qh_triple_table(Triples, Opts1) -->
  {qh_default_table_options(Opts1, Opts2)},
  bs_table(
    \qh_table_header0,
    \html_maplist(qh_triple_row0(Opts2), Triples)
  ).


qh_table_header0 -->
  html(
    tr([
      th(class=subject, "subject"),
      th(class=predicate, "predicate"),
      th(class=object, "object")
    ])
  ).


qh_triple_row0(Opts, rdf(S,P,O)) -->
  html(
    span(class=triple,
      tr([
        td(\qh_subject_outer0(term, [subject], Opts, S)),
        td(\qh_predicate_outer0(predicate, [predicate], Opts, P)),
        td(\qh_object_outer0(term, [object], Opts, O))
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
  qh_triple_table(Triples, Opts2).





% HELPERS %

%! q_dataset_tree(+D, -SumTriples, -Tree) is det.

q_dataset_tree(D, SumTriples, t(dataset_term(D),OrderedTrees)) :-
  aggregate_all(set(G), q_loaded_graph(D, G), Gs),
  maplist(q_graph_tree(D), Gs, NumTriples, Trees),
  pairs_keys_values(Pairs, NumTriples, Trees),
  desc_pairs_values(Pairs, OrderedTrees),
  sum_keys(Pairs, SumTriples).



%! q_graph_tree(+D, +G, -NumTriples, -Tree) is det.

q_graph_tree(D, G, NumTriples, t(graph_term(G),[t([thousands(NumTriples),set(Ms)],[])])) :-
  once((
    q_loaded_graph(M, D, G),
    q_number_of_triples(M, G, NumTriples)
  )),
  aggregate_all(set(M0), q_loaded_graph(M0, D, G), Ms).



%! qh_default_table_options(+Opts1, -Opts2) is det.

qh_default_table_options(Opts1, Opts2) :-
  qh:qh_default_options(_{}, DefOpts1),
  DefOpts2 = _{max_iri_length: 25, qh_link: true},
  merge_dicts(DefOpts1, DefOpts2, DefOpts),
  merge_dicts(Opts1, DefOpts, Opts2).
