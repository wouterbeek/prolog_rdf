:- module(
  qh,
  [
    qh_bnode//1,         %     +B
    qh_bnode//2,         %     +B,             +Opts
    qh_class//2,         % +M, +C
    qh_class//3,         % +M, +C,             +Opts
    qh_datatype//2,      % +M, +D
    qh_datatype//3,      % +M, +D,             +Opts
    qh_describe//2,      % +M, +S
    qh_describe//3,      % +M, +S,             +Opts
    qh_graph_term//2,    % +M, +G
    qh_graph_term//3,    % +M, +G,             +Opts
    qh_graph_table//0,
    qh_graph_table//1,   %                     +Opts
    qh_iri//2,           % +M, +Iri
    qh_iri//3,           % +M, +Iri,           +Opts
    qh_link//4,          % +C, +Cs, +Term, :Content_0
    qh_list//2,          % +M, +L
    qh_list//3,          % +M, +L,             +Opts
    qh_literal//1,       %     +Lit
    qh_literal//2,       %     +Lit,           +Opts
    qh_object//2,        % +M, +O
    qh_object//3,        % +M, +O,             +Opts
    qh_predicate//2,     % +M, +P
    qh_predicate//3,     % +M, +P,             +Opts
    qh_property//2,      % +M, +Prop
    qh_property//3,      % +M, +Prop,          +Opts
    qh_property_path//2, % +M, +Props
    qh_property_path//3, % +M, +Props,         +Opts
    qh_quad//5,          % +M, +S, +P, +O, +G
    qh_quad//6,          % +M, +S, +P, +O, +G, +Opts
    qh_quad_panels//5,   % +M, ?S, ?P, ?O, ?G
    qh_quad_panels//6,   % +M, ?S, ?P, ?O, ?G, +Opts
    qh_quad_table//1,    % +Quads
    qh_quad_table//2,    % +M, +Quads
    qh_quad_table//3,    % +M, +Quads,         +Opts
    qh_quad_table//5,    % +M, ?S, ?P, ?O, ?G
    qh_quad_table//6,    % +M, ?S, ?P, ?O, ?G, +Opts
    qh_subject//2,       % +M, +S
    qh_subject//3,       % +M, +S,             +Opts
    qh_term//2,          % +M, +Term
    qh_term//3,          % +M, +Term,          +Opts
    qh_tree//2,          % +M, +Tree
    qh_tree//3,          % +M, +Tree,          +Opts
    qh_triple//4,        % +M, +S, +P, +O
    qh_triple//5,        % +M, +S, +P, +O,     +Opts
    qh_triple_table//1,  % +Triples
    qh_triple_table//2,  % +M, +Triples
    qh_triple_table//3,  % +M, +Triples,       +Opts
    qh_triple_table//5,  % +M, ?S, ?P, ?O, ?G
    qh_triple_table//6   % +M, ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Quine HTML basics

Generates end user-oriented HTML representations of RDF data.

This assumes that an HTTP handler with id `qh` is defined.

The following options are supported:

  * max_iri_length(+or([nonneg,oneof([inf])]))

  * max_literal_length(+or([nonneg,oneof([inf])]))

---

@author Wouter Beek
@version 2016/02-2016/07
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_bnode_map)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_stmt)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(yall)).

:- html_meta
   qh_link(+, +, +, html, ?, ?),
   qh_link(+, +, +, html, +, ?, ?),
   qh_link(+, +, +, +, html, +, ?, ?).

:- multifile
    qh:qh_literal_hook//2.

:- rdf_meta
   qh_class(+, r, ?, ?),
   qh_class(+, r, +, ?, ?),
   qh_datatype(+, r, ?, ?),
   qh_datatype(+, r, +, ?, ?),
   qh_describe(+, r, ?, ?),
   qh_describe(+, r, +, ?, ?),
   qh_graph_term(+, r, ?, ?),
   qh_graph_term(+, r, +, ?, ?),
   qh_iri(+, r, ?, ?),
   qh_iri(+, r, +, ?, ?),
   qh_list(+, r, ?, ?),
   qh_list(+, r, +, ?, ?),
   qh_literal(o, ?, ?),
   qh_literal(o, +, ?, ?),
   qh_object(+, o, ?, ?),
   qh_object(+, o, +, ?, ?),
   qh_predicate(+, r, ?, ?),
   qh_predicate(+, r, +, ?, ?),
   qh_property(+, r, ?, ?),
   qh_property(+, r, +, ?, ?),
   qh_property_path(+, t, ?, ?),
   qh_property_path(+, t, +, ?, ?),
   qh_quad(+, r, r, o, r, ?, ?),
   qh_quad(+, r, r, o, r, +, ?, ?),
   qh_quad_panels(+, r, r, o, r, ?, ?),
   qh_quad_panels(+, r, r, o, r, +, ?, ?),
   qh_quad_table(+, r, r, o, r, ?, ?),
   qh_quad_table(+, r, r, o, r, +, ?, ?),
   qh_subject(+, r, ?, ?),
   qh_subject(+, r, +, ?, ?),
   qh_term(+, o, ?, ?),
   qh_term(+, o, +, ?, ?),
   qh_triple(+, r, r, o, ?, ?),
   qh_triple(+, r, r, o, +, ?, ?),
   qh_triple_table(+, r, r, o, r, ?, ?),
   qh_triple_table(+, r, r, o, r, +, ?, ?).

:- setting(qh:http_handler, atom, '', "ID of the HTTP handler that performs RDF term lookup.").





%! qh_bnode(+B)// is det.
%! qh_bnode(+B, +Opts)// is det.

qh_bnode(B) -->
  qh_bnode(B, _{}).


qh_bnode(B, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_bnode_outer(_, [], B, Opts2).


qh_bnode_outer(C, Cs1, B, Opts) -->
  {
    ord_add_element(Cs1, bnode, Cs2),
    q_bnode_map(B, Lbl)
  },
  qh_link(C, Cs2, B, \qh_bnode_inner(Lbl, Opts), Opts).


qh_bnode_inner(B, _) -->
  {q_bnode_map(B, Lbl)},
  html(["_:",Lbl]).



%! qh_class(+M, +C)// is det.
%! qh_class(+M, +Cs, +Opts)// is det.

qh_class(M, C) -->
  qh_class(M, C, _{}).


qh_class(M, C, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_class_outer(M, _, [], C, Opts2).


qh_class_outer(M, C0, Cs1, C, Opts) -->
  {ord_add_element(Cs1, class, Cs2)},
  qh_term_outer(M, C0, Cs2, C, Opts).



%! qh_datatype(+M, +D)// is det.
%! qh_datatype(+M, +D, +Opts)// is det.

qh_datatype(M, D) -->
  qh_datatype(M, D, _{}).


qh_datatype(M, D, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_datatype_outer(M, _, [], D, Opts2).


qh_datatype_outer(M, C, Cs1, D, Opts) -->
  {ord_add_element(Cs1, datatype, Cs2)},
  qh_term_outer(M, C, Cs2, D, Opts).



%! qh_describe(+M, +S)// is det.
%! qh_describe(+M, +S, +Opts)// is det.
%
% Generate a full description of subject term S.

qh_describe(M, S) -->
  qh_describe(M, S, _{qh_link: true}).


qh_describe(M, S, Opts1) -->
  {
    qh_default_options(Opts1, Opts2),
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
      td(\qh_property_outer(M, property, [property], P, Opts)),
      td(\html_seplist({M,Opts}/[O]>>qh_object_outer(M, object, [object], O, Opts), " ", Os))
    ])
  ).



%! qh_graph_term(+M, +G)// is det.
%! qh_graph_term(+M, +G, +Opts)// is det.

qh_graph_term(M, G) -->
  qh_graph_term(M, G, _{}).


qh_graph_term(M, G, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_graph_term_outer(M, graph, [], G, Opts2).


qh_graph_term_outer(M, C, Cs1, G, Opts) -->
  {ord_add_element(Cs1, graph, Cs2)},
  qh_iri_outer(M, C, Cs2, G, Opts).



%! qh_graph_table// is det.
%! qh_graph_table(+Opts)// is det.

qh_graph_table -->
  qh_graph_table(_{qh_link: true}).


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
      td(\qh_graph_term_outer(M, graph, [graph], G, Opts)),
      td(\html_thousands(N)),
      td(M)
    ])
  ).



%! qh_iri(+M, +Iri)// is det.
%! qh_iri(+M, +Iri, +Opts)// is det.

qh_iri(M, Iri) -->
  qh_iri(M, Iri, _{}).


qh_iri(M, Iri, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_iri_outer(M, term, [term], Iri, Opts2).


qh_iri_outer(M, C, Cs1, Iri, Opts) -->
  {ord_add_element(Cs1, iri, Cs2)},
  qh_link(C, Cs2, Iri, \qh_iri_inner(M, Iri, Opts), Opts).


% Abbreviated notation for IRI.
qh_iri_inner(_, Iri, Opts) -->
  {rdf_global_id(Alias:Local1, Iri)}, !,
  {dcg_with_output_to(atom(Local2), atom_ellipsis(Local1, Opts.max_iri_length))},
  html([span(class=alias, Alias),":",Local2]).
% RDFS label replacing IRI.
qh_iri_inner(M, Iri, Opts) -->
  {
    ground(M),
    get_dict(iri_lbl, Opts, true),
    q_pref_label(M, Iri, Lbl)
  }, !,
  qh_literal_inner(Lbl, Opts).
% Plain IRI, possibly ellipsed.
qh_iri_inner(_, Iri1, Opts) -->
  {dcg_with_output_to(atom(Iri2), atom_ellipsis(Iri1, Opts.max_iri_length))},
  html(Iri2).



%! qh_list(+M, +L)// is det.
%! qh_list(+M, +L, +Opts)// is det.

qh_list(M, L) -->
  qh_list(M, L, _{}).


qh_list(M, L, Opts1) -->
  {
    qh_default_options(Opts1, Opts2),
    findall(Term, q_list_member(M, L, Term, _), Terms)
  },
  html_list({M,Opts2}/[Term]>>qh_term(M, Term, Opts2), Terms).



%! qh_literal(+Lit)// is det.
%! qh_literal(+Lit, +Opts)// is det.

qh_literal(Lit) -->
  qh_literal(Lit, _{}).


qh_literal(Lit, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_literal_outer(literal, [], Lit, Opts2).


qh_literal_outer(C, Cs1, Lit, Opts) -->
  {
    ord_add_element(Cs1, literal, Cs2),
    q_literal_datatype(Lit, D)
  },
  qh_link(C, Cs2, [datatype=D], Lit, \qh_literal_inner(Lit, Opts), Opts).


% RDF HTML
qh_literal_inner(V^^D, _) -->
  {q_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
qh_literal_inner(Str@LTag, Opts) -->
  {get_dict(show_flag, Opts, true)}, !,
  html([
    span(lang(LTag), \bs_truncated(Str, Opts.max_literal_length)),
    " ",
    \flag_icon(LTag)
  ]).
qh_literal_inner(Str@LTag, Opts) --> !,
  html(span(lang=LTag, \bs_truncated(Str, Opts.max_literal_length))).
% XSD boolean
qh_literal_inner(V^^D, _) -->
  {q_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD gDay
qh_literal_inner(Da^^D, _) -->
  {rdf_equal(xsd:gDay, D)}, !,
  html("~d"-[Da]).
% XSD gMonth
qh_literal_inner(Mo^^D, _) -->
  {rdf_equal(xsd:gMonth, D)}, !,
  html("~d"-[Mo]).
% XSD gYear
qh_literal_inner(Y^^D, _) -->
  {rdf_equal(xsd:gYear, D)}, !,
  html("~d"-[Y]).
% XSD date
% XSD dateTime
% XSD gMonthYear
% XSD gYearMonth
qh_literal_inner(V^^D1, Opts) -->
  {
    q_subdatatype_of(D1, D2),
    rdf11:xsd_date_time_type(D2)
  }, !,
  html_date_time(V, Opts).
% XSD integer
qh_literal_inner(V^^D, _) -->
  {q_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD decimal
qh_literal_inner(V^^D, _) -->
  {q_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD double
% XSD float
qh_literal_inner(V^^D, _) -->
  {(q_subdatatype_of(D, xsd:float) ; q_subdatatype_of(D, xsd:double))}, !,
  html("~G"-[V]).
% XSD string
qh_literal_inner(Str^^D, Opts) -->
  {q_subdatatype_of(D, xsd:string)}, !,
  bs_truncated(Str, Opts.max_literal_length).
% XSD URI
qh_literal_inner(V^^D, _) -->
  {q_subdatatype_of(D, xsd:anyURI)}, !,
  html(V).
% Datatype hooks.
qh_literal_inner(Lit, Opts) -->
  qh:qh_literal_hook(Lit, Opts).
qh_literal_inner(Lit, _) -->
  {gtrace}, %DEB
  html([p(Lit)]).



%! qh_object(+M, +O)// is det.
%! qh_object(+M, +O, +Opts)// is det.

qh_object(M, O) -->
  qh_object(M, O, _{}).


qh_object(M, O, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_object_outer(M, _, [], O, Opts2).


qh_object_outer(M, C, Cs1, O, Opts) -->
  {ord_add_element(Cs1, object, Cs2)},
  qh_term_outer(M, C, Cs2, O, Opts).



%! qh_predicate(+M, +P)// is det.
%! qh_predicate(+M, +P, +Opts)// is det.

qh_predicate(M, P) -->
  qh_predicate(M, P, _{}).


qh_predicate(M, P, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_predicate_outer(M, _, [], P, Opts2).


qh_predicate_outer(M, C, Cs1, P, Opts) -->
  {ord_add_element(Cs1, predicate, Cs2)},
  qh_iri_outer(M, C, Cs2, P, Opts).



%! qh_property(+M, +Prop)// is det.
%! qh_property(+M, +Prop, +Opts)// is det.

qh_property(M, Prop) -->
  qh_property(M, Prop, _{}).


qh_property(M, Prop, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_property_outer(M, _, [], Prop, Opts2).


qh_property_outer(M, C, Cs1, Prop, Opts) -->
  {ord_add_element(Cs1, property, Cs2)},
  qh_term_outer(M, C, Cs2, Prop, Opts).



%! qh_property_path(+M, +Props)// is det.
%! qh_property_path(+M, +Props, +Opts)// is det.

qh_property_path(M, Props) -->
  qh_property_path(M, Props, _{}).


qh_property_path(M, Props, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  html_seplist({M,Opts2}/[Prop]>>qh_property(M, Prop, Opts2), " ", Props).



%! qh_quad(+M, +S, +P, +O, +G)// is det.
%! qh_quad(+M, +S, +P, +O, +G, +Opts)// is det.

qh_quad(M, S, P, O, G) -->
  qh_quad(M, S, P, O, G, _{}).


qh_quad(M, S, P, O, G, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  html(
    span(class=quadruple, [
      &(lang),
      \qh_triple(M, S, P, O, Opts2),
      ", ",
      \qh_graph_term_outer(M, graph, [graph], G, Opts2),
      &(rang)
    ])
  ).



%! qh_quad_panels(+M, ?S, ?P, ?O, ?G)// is det.
%! qh_quad_panels(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

qh_quad_panels(M, S, P, O, G) -->
  qh_quad_panels(M, S, P, O, G, _{}).


qh_quad_panels(M, S, P, O, G, Opts1) -->
  {
    qh_default_options(Opts1, Opts2),
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
        td(\qh_subject_outer(M, term, [subject], S, Opts3)),
        td(\qh_predicate_outer(M, predicate, [predicate], P, Opts3)),
        td(\qh_object_outer(M, term, [object], O, Opts3)),
        td(\qh_graph_term_outer(M, graph, [graph], G, Opts1))
      ])
    )
  ).



%! qh_subject(+M, +S)// is det.
%! qh_subject(+M, +S, +Opts)// is det.

qh_subject(M, S) -->
  qh_subject(M, S, _{}).


qh_subject(M, S, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_subject_outer(M, _, [], S, Opts2).


qh_subject_outer(M, C, Cs1, S, Opts) -->
  {ord_add_element(Cs1, subject, Cs2)},
  (   {q_is_iri(S)}
  ->  qh_iri_outer(M, C, Cs2, S, Opts)
  ;   {q_is_bnode(S)}
  ->  qh_bnode_outer(C, Cs2, S, Opts)
  ).



%! qh_term(+M, +Term)// is det.
%! qh_term(+M, +Term, +Opts)// is det.

qh_term(M, Term) -->
  qh_term(M, Term, _{}).


qh_term(M, Term, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_term_outer(M, _, [], Term, Opts2).


qh_term_outer(M, C, Cs1, Term, Opts) -->
  {ord_add_element(Cs1, term, Cs2)},
  (   {q_is_literal(Term)}
  ->  qh_literal_outer(C, Cs2, Term, Opts)
  ;   {q_is_bnode(Term)}
  ->  qh_bnode_outer(C, Cs2, Term, Opts)
  ;   {q_is_iri(Term)}
  ->  qh_iri_outer(M, C, Cs2, Term, Opts)
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
      \qh_predicate_outer(M, predicate, [predicate], P, Opts),
      " ",
      \qh_object_outer(M, object, [object], Leaf, Opts)
    ]),
    \qh_trees0(M, Ns, Trees, Opts)
  ]).
qh_trees0(M, Ns, [Leaf-[]|Trees], Opts) --> !,
  html([
    div(class=node, \qh_object_outer(M, object, [object], Leaf, Opts)),
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
        \qh_predicate_outer(M, predicate, [predicate], Root, Opts)
      ),
      div(class=tree, \qh_trees0(M, Ns3, Subtrees, Opts))
    ]),
    \qh_trees0(M, Ns2, Trees, Opts)
  ]).



%! qh_triple(+M, +Triple)// is det.
%! qh_triple(+M, +Triple, +Opts)// is det.
%! qh_triple(+M, +S, +P, +O)// is det.
%! qh_triple(+M, +S, +P, +O, +Opts)// is det.

qh_triple(M, Triple) -->
  qh_triple(M, Triple, _{}).


qh_triple(M, rdf(S,P,O), Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_triple(M, S, P, O, Opts2).


qh_triple(M, S, P, O) -->
  qh_triple(M, S, P, O, _{}).


qh_triple(M, S, P, O, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  html(
    span(class=triple, [
      &(lang),
      \qh_subject_outer(M, term, [subject], S, Opts2),
      ", ",
      \qh_predicate_outer(M, predicate, [predicate], P, Opts2),
      ", ",
      \qh_object_outer(M, term, [object], O, Opts2),
      &(rang)
    ])
  ).



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
        td(\qh_subject_outer(M, term, [subject], S, Opts)),
        td(\qh_predicate_outer(M, predicate, [predicate], P, Opts)),
        td(\qh_object_outer(M, term, [object], O, Opts))
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

%! qh_default_options(+Opts1, -Opts2) is det.

qh_default_options(Opts1, Opts2) :-
  DefOpts = _{
    iri_lbl: false,
    max_iri_length: 50,
    max_literal_length: inf,
    qh_link: false
  },
  merge_dicts(DefOpts, Opts1, Opts2).



%! qh_default_table_options(+Opts1, -Opts2) is det.

qh_default_table_options(Opts1, Opts2) :-
  qh_default_options(_{}, DefOpts1),
  DefOpts2 = _{
    max_iri_length: 25,
    qh_link: true
  },
  merge_dicts(DefOpts1, DefOpts2, DefOpts),
  merge_dicts(Opts1, DefOpts, Opts2).



%! qh_link(+C, +Cs, +Term, :Content_0)// is det.
%! qh_link(+C, +Cs, +Term, :Content_0, +Opts)// is det.
%! qh_link(+C, +Cs, +Attrs, +Term, :Content_0, +Opts)// is det.
%
% Generates an RDF request link in case HTTP handler `qh` is
% defined.  Otherwise, the content is generated without an enclosing
% link element.
%
% Cs is a list of classes.  Possible values are `datatype`, `graph`,
% `predicate`, `object`, `subject`, `term`.
%
% C is the class that denotes the context in which Term is displayed.

qh_link(C, Cs, Term, Content_0) -->
  qh_link(C, Cs, Term, Content_0, _{qh_link: true}).


qh_link(C, Cs, Term, Content_0, Opts) -->
  qh_link(C, Cs, [], Term, Content_0, Opts).


qh_link(_, _, _, _, Content_0, Opts) -->
  {Opts.qh_link == false}, !,
  html(Content_0).
qh_link(C, Cs, Attrs, Term, Content_0, Opts) -->
  {
    setting(qh:http_handler, Id),
    Id \== '', !,
    qh_link_query_term(C, Term, QueryTerm),
    (   get_dict(query, Opts, Query0)
    ->  Query = [QueryTerm|Query0]
    ;   Query = [QueryTerm]
    ),
    http_link_to_id(Id, Query, Link)
  },
  html([
    a([class=Cs,href=Link|Attrs], Content_0),
    \qh_link_external(Term)
  ]).
qh_link(_, _, _, _, Content_0, _) -->
  Content_0.


qh_link_query_term(C, Term1, QueryTerm) :-
  term_to_atom(Term1, Term2),
  QueryTerm =.. [C,Term2].


qh_link_external(Term) -->
  {q_is_iri(Term)}, !,
  html([" ",\external_link_icon(Term)]).
qh_link_external(_) --> [].
