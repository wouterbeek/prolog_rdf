:- module(
  zh,
  [
    zh_bnode//1,         %     +B
    zh_bnode//2,         %     +B,             +Opts
    zh_class//2,         % +M, +C
    zh_class//3,         % +M, +C,             +Opts
    zh_datatype//2,      % +M, +D
    zh_datatype//3,      % +M, +D,             +Opts
    zh_describe//2,      % +M, +S
    zh_describe//3,      % +M, +S,             +Opts
    zh_graph_term//2,    % +M, +G
    zh_graph_term//3,    % +M, +G,             +Opts
    zh_graph_table//1,   % +M
    zh_graph_table//2,   % +M,                 +Opts
    zh_iri//2,           % +M, +Iri
    zh_iri//3,           % +M, +Iri,           +Opts
    zh_list//2,          % +M, +L
    zh_list//3,          % +M, +L,             +Opts
    zh_literal//2,       % +M, +Lit
    zh_literal//3,       % +M, +Lit,           +Opts
    zh_object//2,        % +M, +O
    zh_object//3,        % +M, +O,             +Opts
    zh_predicate//2,     % +M, +P
    zh_predicate//3,     % +M, +P,             +Opts
    zh_property//2,      % +M, +Prop
    zh_property//3,      % +M, +Prop,          +Opts
    zh_property_path//2, % +M, +Props
    zh_property_path//3, % +M, +Props,         +Opts
    zh_quad//5,          % +M, +S, +P, +O, +G
    zh_quad//6,          % +M, +S, +P, +O, +G, +Opts
    zh_quad_panels//5,   % +M, ?S, ?P, ?O, ?G
    zh_quad_panels//6,   % +M, ?S, ?P, ?O, ?G, +Opts
    zh_quad_table//5,    % +M, ?S, ?P, ?O, ?G
    zh_quad_table//6,    % +M, ?S, ?P, ?O, ?G, +Opts
    zh_subject//2,       % +M, +S
    zh_subject//3,       % +M, +S,             +Opts
    zh_term//2,          % +M, +Term
    zh_term//3,          % +M, +Term,          +Opts
    zh_tree//2,          % +M, +Tree
    zh_tree//3,          % +M, +Tree,          +Opts
    zh_triple//4,        % +M, +S, +P, +O
    zh_triple//5,        % +M, +S, +P, +O,     +Opts
    zh_triple_table//2,  % +M, +Triples
    zh_triple_table//3,  % +M, +Triples,       +Opts
    zh_triple_table//5,  % +M, ?S, ?P, ?O, ?G
    zh_triple_table//6   % +M, ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> RDF HTML

Generates end user-oriented HTML representations of RDF data.

This assumes that an HTTP handler with id `zh` is defined.

The following options are supported:

  * max_length(+nonneg)

---

@author Wouter Beek
@version 2016/02-2016/06
*/

:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_bnode_map)).
:- use_module(library(rdf/rdf_prefix), []).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(yall)).
:- use_module(library(z/z_datatype)).
:- use_module(library(z/z_stat)).
:- use_module(library(z/z_stmt)).
:- use_module(library(z/z_term)).

:- html_meta
   zh_link(+, +, +, html, +, ?, ?),
   zh_link(+, +, +, +, html, +, ?, ?).

:- multifile
    zh:zh_literal_hook//3.

:- rdf_meta
   zh_class(+, r, ?, ?),
   zh_class(+, r, +, ?, ?),
   zh_datatype(+, r, ?, ?),
   zh_datatype(+, r, +, ?, ?),
   zh_describe(+, r, ?, ?),
   zh_describe(+, r, +, ?, ?),
   zh_graph_term(+, r, ?, ?),
   zh_graph_term(+, r, +, ?, ?),
   zh_iri(+, r, ?, ?),
   zh_iri(+, r, +, ?, ?),
   zh_list(+, r, ?, ?),
   zh_list(+, r, +, ?, ?),
   zh_literal(+, o, ?, ?),
   zh_literal(+, o, +, ?, ?),
   zh_object(+, o, ?, ?),
   zh_object(+, o, +, ?, ?),
   zh_predicate(+, r, ?, ?),
   zh_predicate(+, r, +, ?, ?),
   zh_property(+, r, ?, ?),
   zh_property(+, r, +, ?, ?),
   zh_property_path(+, t, ?, ?),
   zh_property_path(+, t, +, ?, ?),
   zh_quad(+, r, r, o, r, ?, ?),
   zh_quad(+, r, r, o, r, +, ?, ?),
   zh_quad_panels(+, r, r, o, r, ?, ?),
   zh_quad_panels(+, r, r, o, r, +, ?, ?),
   zh_quad_table(+, r, r, o, r, ?, ?),
   zh_quad_table(+, r, r, o, r, +, ?, ?),
   zh_subject(+, r, ?, ?),
   zh_subject(+, r, +, ?, ?),
   zh_term(+, o, ?, ?),
   zh_term(+, o, +, ?, ?),
   zh_triple(+, r, r, o, ?, ?),
   zh_triple(+, r, r, o, +, ?, ?),
   zh_triple_table(+, r, r, o, r, ?, ?),
   zh_triple_table(+, r, r, o, r, +, ?, ?).

:- setting(zh_handler, atom, '',
     "ID of the HTTP handler that performs RDF term lookup."
   ).





%! zh_bnode(+B)// is det.
%! zh_bnode(+B, +Opts)// is det.

zh_bnode(B) -->
  {zh_default_options(Opts)},
  zh_bnode(B, Opts).


zh_bnode(B, Opts) -->
  zh_bnode_outer(_, [], B, Opts).


zh_bnode_outer(C, Cs1, B, Opts) -->
  {
    ord_add_element(Cs1, bnode, Cs2),
    rdf_bnode_map(B, Lbl)
  },
  zh_link(C, Cs2, B, \zh_bnode_inner(Lbl, Opts), Opts).


zh_bnode_inner(B, _) -->
  {rdf_bnode_map(B, Lbl)},
  html(["_:",Lbl]).



%! zh_class(+M, +C)// is det.
%! zh_class(+M, +Cs, +Opts)// is det.

zh_class(M, C) -->
  {zh_default_options(Opts)},
  zh_class(M, C, Opts).


zh_class(M, C, Opts) -->
  zh_class_outer(M, _, [], C, Opts).


zh_class_outer(M, C0, Cs1, C, Opts) -->
  {ord_add_element(Cs1, class, Cs2)},
  zh_term_outer(M, C0, Cs2, C, Opts).



%! zh_datatype(+M, +D)// is det.
%! zh_datatype(+M, +D, +Opts)// is det.

zh_datatype(M, D) -->
  {zh_default_options(Opts)},
  zh_datatype(M, D, Opts).


zh_datatype(M, D, Opts) -->
  zh_datatype_outer(M, _, [], D, Opts).


zh_datatype_outer(M, C, Cs1, D, Opts) -->
  {ord_add_element(Cs1, datatype, Cs2)},
  zh_term_outer(M, C, Cs2, D, Opts).



%! zh_describe(+M, +S)// is det.
%! zh_describe(+M, +S, +Opts)// is det.
%
% Generate a full description of subject term S.

zh_describe(M, S) -->
  {zh_default_options(Opts)},
  zh_describe(M, S, Opts).


zh_describe(M, S, Opts) -->
  {
    findall(P-O, z(M, S, P, O), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  bs_table(
    \bs_table_header(["Predicate","Objects"]),
    \html_maplist({M,Opts}/[Group]>>zh_describe_row0(M, Group, Opts), Groups)
  ).


zh_describe_row0(M, P-Os, Opts) -->
  html(
    tr([
      td(\zh_property_outer(M, property, [property], P, Opts)),
      td(\html_seplist({M,Opts}/[O]>>zh_object(M, O, Opts), " ", Os))
    ])
  ).



%! zh_graph_term(+M, +G)// is det.
%! zh_graph_term(+M, +G, +Opts)// is det.

zh_graph_term(M, G) -->
  {zh_default_options(Opts)},
  zh_graph_term(M, G, Opts).


zh_graph_term(M, G, Opts) -->
  zh_graph_term_outer(M, graph, [], G, Opts).


zh_graph_term_outer(M, C, Cs1, G, Opts) -->
  {ord_add_element(Cs1, graph, Cs2)},
  zh_iri_outer(M, C, Cs2, G, Opts).



%! zh_graph_table(+M)// is det.
%! zh_graph_table(+M, +Opts)// is det.

zh_graph_table(M) -->
  {zh_default_options(Opts)},
  zh_graph_table(M, Opts).


zh_graph_table(M, Opts) -->
  {
    findall(N-G, z_number_of_triples(M, G, N), Pairs),
    desc_pairs_values(Pairs, Gs)
  },
  bs_table(
    \bs_table_header(["Graph","Number of triples"]),
    \html_maplist({M,Opts}/[G]>>zh_graph_row0(M, G, Opts), Gs)
  ).


zh_graph_row0(M, G, Opts) -->
  {z_number_of_triples(M, G, N)},
  html(
    tr([
      td(\zh_graph_term_outer(M, graph, [graph], G, Opts)),
      td(\html_thousands(N))
    ])
  ).



%! zh_iri(+M, +Iri)// is det.
%! zh_iri(+M, +Iri, +Opts)// is det.

zh_iri(M, Iri) -->
  {zh_default_options(Opts)},
  zh_iri(M, Iri, Opts).


zh_iri(M, Iri, Opts) -->
  zh_iri_outer(M, _, [], Iri, Opts).


zh_iri_outer(M, C, Cs1, Iri, Opts) -->
  {ord_add_element(Cs1, iri, Cs2)},
  zh_link(C, Cs2, Iri, \zh_iri_inner(M, Iri, Opts), Opts).


% Abbreviated notation for IRI.
zh_iri_inner(_, Iri, _) -->
  {rdf_global_id(Alias:Local, Iri)}, !,
  html([span(class=alias, Alias),":",Local]).
% RDFS label replacing IRI.
zh_iri_inner(M, Iri, Opts) -->
  {
    get_dict(iri_label, Opts, true),
    z_pref_label(M, Iri, Lbl)
  }, !,
  zh_literal_inner(M, Lbl, Opts).
% Plain IRI.
zh_iri_inner(_, Iri, _) -->
  html(Iri).



%! zh_list(+M, +L)// is det.
%! zh_list(+M, +L, +Opts)// is det.

zh_list(M, L) -->
  {zh_default_options(Opts)},
  zh_list(M, L, Opts).


zh_list(M, L, Opts) -->
  {z_list_pl(M, L, Terms)},
  list({M,Opts}/[Term]>>zh_term(M, Term, Opts), Terms).



%! zh_literal(+M, +Lit)// is det.
%! zh_literal(+M, +Lit, +Opts)// is det.

zh_literal(M, Lit) -->
  {zh_default_options(Opts)},
  zh_literal(M, Lit, Opts).


zh_literal(M, Lit, Opts) -->
  zh_literal_outer(M, _, [], Lit, Opts).


zh_literal_outer(M, C, Cs1, Lit, Opts) -->
  {
    ord_add_element(Cs1, literal, Cs2),
    z_literal_datatype(Lit, D)
  },
  zh_link(C, Cs2, [datatype=D], Lit, \zh_literal_inner(M, Lit, Opts), Opts).


% RDF HTML
zh_literal_inner(_, V^^D, _) -->
  {z_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
zh_literal_inner(_, Str@LTag, Opts) -->
  {get_dict(show_flag, Opts, true)}, !,
  html([
    span(lang(LTag), \bs_truncated(Str, Opts.max_length)),
    " ",
    \flag_icon(LTag)
  ]).
zh_literal_inner(_, Str@LTag, Opts) --> !,
  html(span(lang=LTag, \bs_truncated(Str, Opts.max_length))).
% XSD boolean
zh_literal_inner(_, V^^D, _) -->
  {z_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD gDay
zh_literal_inner(_, Da^^D, _) -->
  {rdf_equal(xsd:gDay, D)}, !,
  html("~d"-[Da]).
% XSD gMonth
zh_literal_inner(_, Mo^^D, _) -->
  {rdf_equal(xsd:gMonth, D)}, !,
  html("~d"-[Mo]).
% XSD gYear
zh_literal_inner(_, Y^^D, _) -->
  {rdf_equal(xsd:gYear, D)}, !,
  html("~d"-[Y]).
% XSD date
% XSD dateTime
% XSD gMonthYear
% XSD gYearMonth
zh_literal_inner(_, V^^D1, Opts) -->
  {
    z_subdatatype_of(D1, D2),
    rdf11:xsd_date_time_type(D2)
  }, !,
  html_date_time(V, Opts).
% XSD integer
zh_literal_inner(_, V^^D, _) -->
  {z_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD decimal
zh_literal_inner(_, V^^D, _) -->
  {z_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD double
% XSD float
zh_literal_inner(_, V^^D, _) -->
  {(  z_subdatatype_of(D, xsd:float)
  ;   z_subdatatype_of(D, xsd:double)
  )}, !,
  html("~G"-[V]).
% XSD string
zh_literal_inner(_, Str^^D, Opts) -->
  {z_subdatatype_of(D, xsd:string)}, !,
  bs_truncated(Str, Opts.max_length).
% XSD URI
zh_literal_inner(_, V^^D, _) -->
  {z_subdatatype_of(D, xsd:anyURI)}, !,
  html(V).
% Datatype hooks.
zh_literal_inner(M, Lit, Opts) -->
  zh:zh_literal_hook(M, Lit, Opts).
zh_literal_inner(_, Lit, _) -->
  {gtrace}, %DEB
  html([p(Lit)]).



%! zh_object(+M, +O)// is det.
%! zh_object(+M, +O, +Opts)// is det.

zh_object(M, O) -->
  {zh_default_options(Opts)},
  zh_object(M, O, Opts).


zh_object(M, O, Opts) -->
  zh_object_outer(M, _, [], O, Opts).


zh_object_outer(M, C, Cs1, O, Opts) -->
  {ord_add_element(Cs1, object, Cs2)},
  zh_term_outer(M, C, Cs2, O, Opts).



%! zh_predicate(+M, +P)// is det.
%! zh_predicate(+M, +P, +Opts)// is det.

zh_predicate(M, P) -->
  {zh_default_options(Opts)},
  zh_predicate(M, P, Opts).


zh_predicate(M, P, Opts) -->
  zh_predicate_outer(M, _, [], P, Opts).


zh_predicate_outer(M, C, Cs1, P, Opts) -->
  {ord_add_element(Cs1, predicate, Cs2)},
  zh_iri_outer(M, C, Cs2, P, Opts).



%! zh_property(+M, +Prop)// is det.
%! zh_property(+M, +Prop, +Opts)// is det.

zh_property(M, Prop) -->
  {zh_default_options(Opts)},
  zh_property(M, Prop, Opts).


zh_property(M, Prop, Opts) -->
  zh_property_outer(M, _, [], Prop, Opts).


zh_property_outer(M, C, Cs1, Prop, Opts) -->
  {ord_add_element(Cs1, property, Cs2)},
  zh_term_outer(M, C, Cs2, Prop, Opts).



%! zh_property_path(+M, +Props)// is det.
%! zh_property_path(+M, +Props, +Opts)// is det.

zh_property_path(M, Props) -->
  {zh_default_options(Opts)},
  zh_property_path(M, Props, Opts).


zh_property_path(M, Props, Opts) -->
  html_seplist({M,Opts}/[Prop]>>zh_property(M, Prop, Opts), " ", Props).



%! zh_quad(+M, +S, +P, +O, +G)// is det.
%! zh_quad(+M, +S, +P, +O, +G, +Opts)// is det.

zh_quad(M, S, P, O, G) -->
  {zh_default_options(Opts)},
  zh_quad(M, S, P, O, G, Opts).


zh_quad(M, S, P, O, G, Opts) -->
  html(
    span(class=quadruple, [
      &(lang),
      \zh_triple(M, S, P, O, Opts),
      ", ",
      \zh_graph_term_outer(M, graph, [graph], G, Opts),
      &(rang)
    ])
  ).



%! zh_quad_panels(+M, ?S, ?P, ?O, ?G)// is det.
%! zh_quad_panels(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

zh_quad_panels(M, S, P, O, G) -->
  {zh_default_options(Opts)},
  zh_quad_panels(M, S, P, O, G, Opts).


zh_quad_panels(M, S, P, O, G, Opts) -->
  {
    findall(G-Triple, z_triple(M, S, P, O, G, Triple), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  bs_panels({M,Opts}/[Group]>>zh_triple_table(M, Group, Opts), Groups).



%! zh_quad_row(+M, +Quad)// is det.

zh_quad_row(M, Quad) -->
  {zh_default_options(Opts)},
  zh_quad_row(M, Quad, Opts).


zh_quad_row(M, rdf(S,P,O,G), Opts) -->
  html(
    span(class=quadruple,
      tr([
        td(\zh_subject_outer(M, subject, [subject], S, Opts)),
        td(\zh_predicate_outer(M, predicate, [predicate], P, Opts)),
        td(\zh_object_outer(M, object, [object], O, Opts)),
        td(\zh_graph_term_outer(M, graph, [graph], G, Opts))
      ])
    )
  ).



%! zh_quad_table(+M, +Quads)// is det.
%! zh_quad_table(+M, +Quads, +Opts)// is det.
%! zh_quad_table(+M, ?S, ?P, ?O, ?G)// is det.
%! zh_quad_table(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

zh_quad_table(M, Quads) -->
  {zh_default_options(Opts)},
  zh_quad_table(M, Quads, Opts).


zh_quad_table(M, Quads, Opts) -->
  bs_table(
    \bs_table_header(["Subject","Predicate","Object","Graph"]),
    \html_maplist({M,Opts}/[Quad]>>zh_quad_row(M, Quad, Opts), Quads)
  ).


zh_quad_table(M, S, P, O, G) -->
  {zh_default_options(Opts)},
  zh_quad_table(M, S, P, O, G, Opts).


zh_quad_table(M, S, P, O, G, Opts) -->
  {z_quads(M, S, P, O, G, Quads)},
  zh_quad_table(M, Quads, Opts).



%! zh_subject(+M, +S)// is det.
%! zh_subject(+M, +S, +Opts)// is det.

zh_subject(M, S) -->
  {zh_default_options(Opts)},
  zh_subject(M, S, Opts).


zh_subject(M, S, Opts) -->
  zh_subject_outer(M, _, [], S, Opts).


zh_subject_outer(M, C, Cs1, S, Opts) -->
  {ord_add_element(Cs1, subject, Cs2)},
  (   {rdf_is_iri(S)}
  ->  zh_iri_outer(M, C, Cs2, S, Opts)
  ;   {rdf_is_bnode(S)}
  ->  zh_bnode_outer(C, Cs2, S, Opts)
  ).



%! zh_term(+M, +Term)// is det.
%! zh_term(+M, +Term, +Opts)// is det.

zh_term(M, Term) -->
  {zh_default_options(Opts)},
  zh_term(M, Term, Opts).


zh_term(M, Term, Opts) -->
  zh_term_outer(M, _, [], Term, Opts).


zh_term_outer(M, C, Cs1, Term, Opts) -->
  {ord_add_element(Cs1, term, Cs2)},
  (   {rdf_is_literal(Term)}
  ->  zh_literal_outer(M, C, Cs2, Term, Opts)
  ;   {rdf_is_bnode(Term)}
  ->  zh_bnode_outer(C, Cs2, Term, Opts)
  ;   {rdf_is_iri(Term)}
  ->  zh_iri_outer(M, C, Cs2, Term, Opts)
  ).



%! zh_tree(+M, +Tree)// is det.
%! zh_tree(+M, +Tree, +Opts)// is det.

zh_tree(M, Tree) -->
  {zh_default_options(Opts)},
  zh_tree(M, Tree, Opts).


zh_tree(M, Tree, Opts) -->
  html([
    \check_all0,
    div(class(treeview), div(class(tree),\zh_trees0(M, [0], [Tree], Opts)))
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


zh_trees0(_, _, [], _) --> !, [].
zh_trees0(M, Ns, [P-[Leaf-[]]|Trees], Opts) --> !,
  html([
    div(class=node, [
      \zh_predicate_outer(M, predicate, [predicate], P, Opts),
      " ",
      \zh_object_outer(M, object, [object], Leaf, Opts)
    ]),
    \zh_trees0(M, Ns, Trees, Opts)
  ]).
zh_trees0(M, Ns, [Leaf-[]|Trees], Opts) --> !,
  html([
    div(class=node, \zh_object_outer(M, object, [object], Leaf, Opts)),
    \zh_trees0(M, Ns, Trees, Opts)
  ]).
zh_trees0(M, Ns1, [Root-Subtrees|Trees], Opts) -->
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
        \zh_predicate_outer(M, predicate, [predicate], Root, Opts)
      ),
      div(class=tree, \zh_trees0(M, Ns3, Subtrees, Opts))
    ]),
    \zh_trees0(M, Ns2, Trees, Opts)
  ]).



%! zh_triple(+M, +Triple)// is det.
%! zh_triple(+M, +Triple, +Opts)// is det.
%! zh_triple(+M, +S, +P, +O)// is det.
%! zh_triple(+M, +S, +P, +O, +Opts)// is det.

zh_triple(M, Triple) -->
  {zh_default_options(Opts)},
  zh_triple(M, Triple, Opts).


zh_triple(M, rdf(S,P,O), Opts) -->
  zh_triple(M, S, P, O, Opts).


zh_triple(M, S, P, O) -->
  {zh_default_options(Opts)},
  zh_triple(M, S, P, O, Opts).


zh_triple(M, S, P, O, Opts) -->
  html(
    span(class=triple, [
      &(lang),
      \zh_subject_outer(M, subject, [subject], S, Opts),
      ", ",
      \zh_predicate_outer(M, predicate, [predicate], P, Opts),
      ", ",
      \zh_object_outer(M, object, [object], O, Opts),
      &(rang)
    ])
  ).



%! zh_triple_row(+M, +Triple)// is det.
%! zh_triple_row(+M, +Triple, +Opts)// is det.

zh_triple_row(M, Triple) -->
  {zh_default_options(Opts)},
  zh_triple_row(M, Triple, Opts).


zh_triple_row(M, rdf(S,P,O), Opts) -->
  html(
    span(class=triple,
      tr([
        td(\zh_subject_outer(M, subject, [subject], S, Opts)),
        td(\zh_predicate_outer(M, predicate, [predicate], P, Opts)),
        td(\zh_object_outer(M, object, [object], O, Opts))
      ])
    )
  ).



%! zh_triple_table(+M, +Triples)// is det.
%! zh_triple_table(+M, +Triples, +Opts)// is det.

zh_triple_table(M, Triples) -->
  {zh_default_options(Opts)},
  zh_triple_table(M, Triples, Opts).


zh_triple_table(M, Triples, Opts) -->
  bs_table(
    \zh_table_header0,
    \html_maplist({M,Opts}/[Triple]>>zh_triple_row(M, Triple, Opts), Triples)
  ).


zh_table_header0 -->
  html(
    tr([
      th(class=subject, "Subject"),
      th(class=predicate, "Predicate"),
      th(class=object, "Object")
    ])
  ).



%! zh_triple_table(+M, ?S, ?P, ?O, ?G)// is det.
%! zh_triple_table(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

zh_triple_table(M, S, P, O, G) -->
  {zh_default_options(Opts)},
  zh_triple_table(M, S, P, O, G, Opts).


zh_triple_table(M, S, P, O, G, Opts) -->
  {z_triples(M, S, P, O, G, Triples)},
  zh_triple_table(M, Triples, Opts).





% HELPERS %

zh_default_options(_{max_length: 50}).



%! zh_link(+C, +Cs, +Term, :Content_0, +Opts)// is det.
%! zh_link(+C, +Cs, +Attrs, +Term, :Content_0, +Opts)// is det.
%
% Generates an RDF request link in case HTTP handler `zh` is
% defined.  Otherwise, the content is generated without an enclosing
% link element.
%
% Cs is a list of classes.  Possible values are `datatype`, `graph`,
% `predicate`, `object`, `subject`, `term`.
%
% C is the class that denotes the context in which Term is displayed.

zh_link(C, Cs, Term, Content_0, Opts) -->
  zh_link(C, Cs, [], Term, Content_0, Opts).


zh_link(C, Cs, Attrs, Term, Content_0, Opts) -->
  {
    setting(zh_handler, Id),
    Id \== '', !,
    zh_link_query_term(C, Term, QueryTerm),
    (   get_dict(query, Opts, Query0)
    ->  Query = [QueryTerm|Query0]
    ;   Query = [QueryTerm]
    ),
    http_link_to_id(Id, Query, Link)
  },
  html([
    html(a([class=Cs,href=Link|Attrs], Content_0)),
    \zh_link_external(Term)
  ]).
zh_link(_, _, _, _, Content_0, _) -->
  Content_0.


zh_link_query_term(C, Term1, QueryTerm) :-
  term_to_atom(Term1, Term2),
  QueryTerm =.. [C,Term2].


zh_link_external(Term) -->
  {rdf_is_iri(Term)}, !,
  html([" ",\external_link_icon(Term)]).
zh_link_external(_) --> [].
