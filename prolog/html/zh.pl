:- module(
  zh,
  [
    zh_bnode//1,         % +B
    zh_bnode//2,         % +B, +Opts
    zh_class//1,         % +C
    zh_class//2,         % +C, +Opts
    zh_datatype//1,      % +D
    zh_datatype//2,      % +D, +Opts
    zh_describe//1,      % +S
    zh_graph//1,         % +G
    zh_graph_table//0,
    zh_iri//1,           % +Iri
    zh_list//1,          % +L
    zh_list//2,          % +L, +Opts
    zh_literal//1,       % +Lit
    zh_literal//2,       % +Lit, +Opts
    zh_object//1,        % +O
    zh_object//2,        % +O, +Opts
    zh_predicate//1,     % +P
    zh_predicate//2,     % +P, +Opts
    zh_property//1,      % +Prop
    zh_property//2,      % +Prop, +Opts
    zh_property_path//1, % +Props
    zh_property_path//2, % +Props, +Opts
    zh_quad//4,          % +S, +P, +O, +G
    zh_quad_panels//4,   % ?S, ?P, ?O, ?G
    zh_quad_panels//5,   % ?S, ?P, ?O, ?G, +Opts
    zh_quad_table//4,    % ?S, ?P, ?O, ?G
    zh_quad_table//5,    % ?S, ?P, ?O, ?G, +Opts
    zh_subject//1,       % +S
    zh_subject//2,       % +S, +Opts
    zh_term//1,          % +Term
    zh_term//2,          % +Term, +Opts
    zh_tree//1,          % +Tree
    zh_tree//2,          % +Tree, +Opts
    zh_triple//3,        % +S, +P, +O
    zh_triple//4,        % +S, +P, +O, +Opts
    zh_triple_table//1,  % +Triples
    zh_triple_table//2,  % +Triples, +Opts
    zh_triple_table//4,  % ?S, ?P, ?O, ?G
    zh_triple_table//5   % +S, ?P, ?O, ?G, +Opts
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
:- use_module(library(rdfs/rdfs_ext)).
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
    zh:zh_literal_hook//2.

:- rdf_meta
   zh_quad_panels(r, r, o, r, ?, ?),
   zh_quad_panels(r, r, o, r, +, ?, ?),
   zh_quad_table(r, r, o, r, ?, ?),
   zh_quad_table(r, r, o, r, +, ?, ?),
   zh_triple_table(r, r, o, r, ?, ?),
   zh_triple_table(r, r, o, r, +, ?, ?).

:- setting(zh_handler, atom, '',
     "ID of the HTTP handler that performs RDF term lookup."
   ).





%! zh_bnode(+BNode)// is det.
%! zh_bnode(+BNode, +Opts)// is det.

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



%! zh_class(+C)// is det.
%! zh_class(+Cs, +Opts)// is det.

zh_class(C) -->
  {zh_default_options(Opts)},
  zh_class(C, Opts).


zh_class(C, Opts) -->
  zh_class_outer(_, [], C, Opts).


zh_class_outer(C0, Cs1, C, Opts) -->
  {ord_add_element(Cs1, class, Cs2)},
  zh_term_outer(C0, Cs2, C, Opts).



%! zh_datatype(+D)// is det.
%! zh_datatype(+D, +Opts)// is det.

zh_datatype(D) -->
  {zh_default_options(Opts)},
  zh_datatype(D, Opts).


zh_datatype(D, Opts) -->
  zh_datatype_outer(_, [], D, Opts).


zh_datatype_outer(C, Cs1, D, Opts) -->
  {ord_add_element(Cs1, datatype, Cs2)},
  zh_term_outer(C, Cs2, D, Opts).



%! zh_describe(+S)// is det.
%! zh_describe(+S, +Opts)// is det.
%
% Generate a full description of subject term S.

zh_describe(S) -->
  {zh_default_options(Opts)},
  zh_describe(S, Opts).


zh_describe(S, Opts) -->
  {
    findall(P-O, z(S, P, O), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  bs_table(
    \bs_table_header(["Predicate","Objects"]),
    \html_maplist({Opts}/[Group]>>zh_describe_row0(Group, Opts), Groups)
  ).


zh_describe_row0(P-Os, Opts) -->
  html(
    tr([
      td(\zh_property_outer(property, [property], P, Opts)),
      td(\html_seplist({Opts}/[O]>>zh_object(O, Opts), " ", Os))
    ])
  ).



%! zh_graph(+G)// is det.
%! zh_graph(+G, +Opts)// is det.

zh_graph(G) -->
  {zh_default_options(Opts)},
  zh_graph(G, Opts).


zh_graph(G, Opts) -->
  zh_graph_outer(_, [], G, Opts).


zh_graph_outer(C, Cs1, G, Opts) -->
  {ord_add_element(Cs1, graph, Cs2)},
  zh_iri_outer(C, Cs2, G, Opts).



%! zh_graph_table// is det.
%! zh_graph_table(+Opts)// is det.

zh_graph_table -->
  {zh_default_options(Opts)},
  zh_graph_table(Opts).


zh_graph_table(Opts) -->
  {
    findall(N-G, z_number_of_triples(G, N), Pairs),
    desc_pairs_values(Pairs, Gs)
  },
  bs_table(
    \bs_table_header(["Graph","Number of triples"]),
    \html_maplist({Opts}/[G]>>zh_graph_row0(G, Opts), Gs)
  ).


zh_graph_row0(G, Opts) -->
  {z_number_of_triples(G, N)},
  html(
    tr([
      td(\zh_graph_outer(graph, [graph], G, Opts)),
      td(\html_thousands(N))
    ])
  ).



%! zh_iri(+Iri)// is det.
%! zh_iri(+Iri, +Opts)// is det.

zh_iri(Iri) -->
  {zh_default_options(Opts)},
  zh_iri(Iri, Opts).


zh_iri(Iri, Opts) -->
  zh_iri_outer(_, [], Iri, Opts).


zh_iri_outer(C, Cs1, Iri, Opts) -->
  {ord_add_element(Cs1, iri, Cs2)},
  zh_link(C, Cs2, Iri, \zh_iri_inner(Iri, Opts), Opts).


% Abbreviated notation for IRI.
zh_iri_inner(Iri, _) -->
  {rdf_global_id(Alias:Local, Iri)}, !,
  html([span(class=alias, Alias),":",Local]).
% RDFS label replacing IRI.
zh_iri_inner(Iri, Opts) -->
  {
    get_dict(iri_label, Opts, true),
    rdfs_pref_label(Iri, Lbl)
  }, !,
  zh_literal_inner(Lbl, Opts).
% Plain IRI.
zh_iri_inner(Iri, _) -->
  html(Iri).



%! zh_list(+L)// is det.
%! zh_list(+L, ?G)// is det.
%! zh_list(+L, +Opts)// is det.

zh_list(L) -->
  {zh_default_options(Opts)},
  zh_list(L, Opts).


zh_list(L, Opts) -->
  {rdf_list(L, Terms)},
  list({Opts}/[Term]>>zh_term(Term, Opts), Terms).



%! zh_literal(+Lit)// is det.
%! zh_literal(+Lit, +Opts)// is det.

zh_literal(Lit) -->
  {zh_default_options(Opts)},
  zh_literal(Lit, Opts).


zh_literal(Lit, Opts) -->
  zh_literal_outer(_, [], Lit, Opts).


zh_literal_outer(C, Cs1, Lit, Opts) -->
  {
    ord_add_element(Cs1, literal, Cs2),
    z_literal_datatype(Lit, D)
  },
  zh_link(C, Cs2, [datatype=D], Lit, \zh_literal_inner(Lit, Opts), Opts).


% RDF HTML
zh_literal_inner(V^^D, _) -->
  {z_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
zh_literal_inner(Str@LTag, Opts) -->
  {get_dict(show_flag, Opts, true)}, !,
  html([
    span(lang(LTag), \bs_truncated(Str, Opts.max_length)),
    " ",
    \flag_icon(LTag)
  ]).
zh_literal_inner(Str@LTag, Opts) --> !,
  html(span(lang=LTag, \bs_truncated(Str, Opts.max_length))).
% XSD boolean
zh_literal_inner(V^^D, _) -->
  {z_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD gDay
zh_literal_inner(Da^^D, _) -->
  {rdf_equal(xsd:gDay, D)}, !,
  html("~d"-[Da]).
% XSD gMonth
zh_literal_inner(Mo^^D, _) -->
  {rdf_equal(xsd:gMonth, D)}, !,
  html("~d"-[Mo]).
% XSD gYear
zh_literal_inner(Y^^D, _) -->
  {rdf_equal(xsd:gYear, D)}, !,
  html("~d"-[Y]).
% XSD date
% XSD dateTime
% XSD gMonthYear
% XSD gYearMonth
zh_literal_inner(V^^D1, Opts) -->
  {
    z_subdatatype_of(D1, D2),
    rdf11:xsd_date_time_type(D2)
  }, !,
  html_date_time(V, Opts).
% XSD integer
zh_literal_inner(V^^D, _) -->
  {z_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD decimal
zh_literal_inner(V^^D, _) -->
  {z_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD double
% XSD float
zh_literal_inner(V^^D, _) -->
  {(  z_subdatatype_of(D, xsd:float)
  ;   z_subdatatype_of(D, xsd:double)
  )}, !,
  html("~G"-[V]).
% XSD string
zh_literal_inner(Str^^D, Opts) -->
  {z_subdatatype_of(D, xsd:string)}, !,
  bs_truncated(Str, Opts.max_length).
% XSD URI
zh_literal_inner(V^^D, _) -->
  {z_subdatatype_of(D, xsd:anyURI)}, !,
  html(V).
% Datatype hooks.
zh_literal_inner(Lit, Opts) -->
  zh:zh_literal_hook(Lit, Opts).
zh_literal_inner(Lit, _) -->
  {gtrace}, %DEB
  html([p(Lit)]).



%! zh_object(+O)// is det.
%! zh_object(+O, +Opts)// is det.

zh_object(O) -->
  {zh_default_options(Opts)},
  zh_object(O, Opts).


zh_object(O, Opts) -->
  zh_object_outer(_, [], O, Opts).


zh_object_outer(C, Cs1, O, Opts) -->
  {ord_add_element(Cs1, object, Cs2)},
  zh_term_outer(C, Cs2, O, Opts).



%! zh_predicate(+P)// is det.
%! zh_predicate(+P, +Opts)// is det.

zh_predicate(P) -->
  {zh_default_options(Opts)},
  zh_predicate(P, Opts).


zh_predicate(P, Opts) -->
  zh_predicate_outer(_, [], P, Opts).


zh_predicate_outer(C, Cs1, P, Opts) -->
  {ord_add_element(Cs1, predicate, Cs2)},
  zh_iri_outer(C, Cs2, P, Opts).



%! zh_property(+Prop)// is det.
%! zh_property(+Prop, +Opts)// is det.

zh_property(Prop) -->
  {zh_default_options(Opts)},
  zh_property(Prop, Opts).


zh_property(Prop, Opts) -->
  zh_property_outer(_, [], Prop, Opts).


zh_property_outer(C, Cs1, Prop, Opts) -->
  {ord_add_element(Cs1, property, Cs2)},
  zh_term_outer(C, Cs2, Prop, Opts).



%! zh_property_path(+Props       )// is det.
%! zh_property_path(+Props, +Opts)// is det.

zh_property_path(Props) -->
  {zh_default_options(Opts)},
  zh_property_path(Props, Opts).


zh_property_path(Props, Opts) -->
  html_seplist({Opts}/[Prop]>>zh_property(Prop, Opts), " ", Props).



%! zh_quad(+S, +P, +O, +G)// is det.

zh_quad(S, P, O, G) -->
  {zh_default_options(Opts)},
  zh_quad(S, P, O, G, Opts).


zh_quad(S, P, O, G, Opts) -->
  html(
    span(class=quadruple, [
      &(lang),
      \zh_triple(S, P, O, Opts),
      ", ",
      \zh_graph_outer(graph, [graph], G, Opts),
      &(rang)
    ])
  ).



%! zh_quad_panels(?S, ?P, ?O, ?G)// is det.
%! zh_quad_panels(?S, ?P, ?O, ?G, +Opts)// is det.

zh_quad_panels(S, P, O, G) -->
  {zh_default_options(Opts)},
  zh_quad_panels(S, P, O, G, Opts).


zh_quad_panels(S, P, O, G, Opts) -->
  {
    findall(G-Triple, z_triple(S, P, O, G, Triple), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  bs_panels({Opts}/[Group]>>zh_triple_table(Group, Opts), Groups).



%! zh_quad_row(+Quad)// is det.

zh_quad_row(Quad) -->
  {zh_default_options(Opts)},
  zh_quad_row(Quad, Opts).


zh_quad_row(rdf(S,P,O,G), Opts) -->
  html(
    span(class=quadruple,
      tr([
        td(\zh_subject_outer(subject, [subject], S, Opts)),
        td(\zh_predicate_outer(predicate, [predicate], P, Opts)),
        td(\zh_object_outer(object, [object], O, Opts)),
        td(\zh_graph_outer(graph, [graph], G, Opts))
      ])
    )
  ).



%! zh_quad_table(+Quads)// is det.
%! zh_quad_table(+Quads, +Opts)// is det.
%! zh_quad_table(?S, ?P, ?O, ?G)// is det.
%! zh_quad_table(?S, ?P, ?O, ?G, +Opts)// is det.

zh_quad_table(Quads) -->
  {zh_default_options(Opts)},
  zh_quad_table(Quads, Opts).


zh_quad_table(Quads, Opts) -->
  bs_table(
    \bs_table_header(["Subject","Predicate","Object","Graph"]),
    \html_maplist({Opts}/[Quad]>>zh_quad_row(Quad, Opts), Quads)
  ).


zh_quad_table(S, P, O, G) -->
  {zh_default_options(Opts)},
  zh_quad_table(S, P, O, G, Opts).


zh_quad_table(S, P, O, G, Opts) -->
  {z_quads(S, P, O, G, Quads)},
  zh_quad_table(Quads, Opts).



%! zh_subject(+S)// is det.
%! zh_subject(+S, +Opts)// is det.

zh_subject(S) -->
  {zh_default_options(Opts)},
  zh_subject(S, Opts).


zh_subject(S, Opts) -->
  zh_subject_outer(_, [], S, Opts).


zh_subject_outer(C, Cs1, S, Opts) -->
  {ord_add_element(Cs1, subject, Cs2)},
  (   {rdf_is_iri(S)}
  ->  zh_iri_outer(C, Cs2, S, Opts)
  ;   {rdf_is_bnode(S)}
  ->  zh_bnode_outer(C, Cs2, S, Opts)
  ).



%! zh_term(+Term)// is det.
%! zh_term(+Term, +Opts)// is det.

zh_term(Term) -->
  {zh_default_options(Opts)},
  zh_term(Term, Opts).


zh_term(Term, Opts) -->
  zh_term_outer(_, [], Term, Opts).


zh_term_outer(C, Cs1, Term, Opts) -->
  {ord_add_element(Cs1, term, Cs2)},
  (   {rdf_is_literal(Term)}
  ->  zh_literal_outer(C, Cs2, Term, Opts)
  ;   {rdf_is_bnode(Term)}
  ->  zh_bnode_outer(C, Cs2, Term, Opts)
  ;   {rdf_is_iri(Term)}
  ->  zh_iri_outer(C, Cs2, Term, Opts)
  ).



%! zh_tree(+Tree)// is det.
%! zh_tree(+Tree, +Opts)// is det.

zh_tree(Tree) -->
  {zh_default_options(Opts)},
  zh_tree(Tree, Opts).


zh_tree(Tree, Opts) -->
  html([
    \check_all0,
    div(class(treeview), div(class(tree),\zh_trees0([0], [Tree], Opts)))
  ]).


check_all0 -->
  html([
    \js_script({|javascript(_)||
$("#checkAll").change(function () {
  $("input:checkbox").prop('checked', $(this).prop("checked"));
});
    |}),
    p(label([input([id(checkAll),type(checkbox)], []), 'Check all']))
  ]).


zh_trees0(_, [], _) --> !, [].
zh_trees0(Ns, [P-[Leaf-[]]|Trees], Opts) --> !,
  html([
    div(class=node, [
      \zh_predicate_outer(predicate, [predicate], P, Opts),
      " ",
      \zh_object_outer(object, [object], Leaf, Opts)
    ]),
    \zh_trees0(Ns, Trees, Opts)
  ]).
zh_trees0(Ns, [Leaf-[]|Trees], Opts) --> !,
  html([
    div(class=node, \zh_object_outer(object, [object], Leaf, Opts)),
    \zh_trees0(Ns, Trees, Opts)
  ]).
zh_trees0(Ns1, [Root-Subtrees|Trees], Opts) -->
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
      label(for=Id, \zh_predicate_outer(predicate, [predicate], Root, Opts)),
      div(class=tree, \zh_trees0(Ns3, Subtrees, Opts))
    ]),
    \zh_trees0(Ns2, Trees, Opts)
  ]).



%! zh_triple(+Triple)// is det.
%! zh_triple(+Triple, +Opts)// is det.
%! zh_triple(+S, +P, +O)// is det.
%! zh_triple(+S, +P, +O, +Opts)// is det.

zh_triple(Triple) -->
  {zh_default_options(Opts)},
  zh_triple(Triple, Opts).


zh_triple(rdf(S,P,O), Opts) -->
  zh_triple(S, P, O, Opts).


zh_triple(S, P, O) -->
  {zh_default_options(Opts)},
  zh_triple(S, P, O, Opts).


zh_triple(S, P, O, Opts) -->
  html(
    span(class=triple, [
      &(lang),
      \zh_subject_outer(subject, [subject], S, Opts),
      ", ",
      \zh_predicate_outer(predicate, [predicate], P, Opts),
      ", ",
      \zh_object_outer(object, [object], O, Opts),
      &(rang)
    ])
  ).



%! zh_triple_row(+Triple)// is det.
%! zh_triple_row(+Triple, +Opts)// is det.

zh_triple_row(Triple) -->
  {zh_default_options(Opts)},
  zh_triple_row(Triple, Opts).


zh_triple_row(rdf(S,P,O), Opts) -->
  html(
    span(class=triple,
      tr([
        td(\zh_subject_outer(subject, [subject], S, Opts)),
        td(\zh_predicate_outer(predicate, [predicate], P, Opts)),
        td(\zh_object_outer(object, [object], O, Opts))
      ])
    )
  ).



%! zh_triple_table(+Triples)// is det.
%! zh_triple_table(+Triples, +Opts)// is det.

zh_triple_table(Triples) -->
  {zh_default_options(Opts)},
  zh_triple_table(Triples, Opts).


zh_triple_table(Triples, Opts) -->
  bs_table(
    \zh_table_header0,
    \html_maplist({Opts}/[Triple]>>zh_triple_row(Triple, Opts), Triples)
  ).


zh_table_header0 -->
  html(
    tr([
      th(class=subject, "Subject"),
      th(class=predicate, "Predicate"),
      th(class=object, "Object")
    ])
  ).



%! zh_triple_table(?S, ?P, ?O, ?G)// is det.
%! zh_triple_table(?S, ?P, ?O, ?G, +Opts)// is det.

zh_triple_table(S, P, O, G) -->
  {zh_default_options(Opts)},
  zh_triple_table(S, P, O, G, Opts).


zh_triple_table(S, P, O, G, Opts) -->
  {z_triples(S, P, O, G, Triples)},
  zh_triple_table(Triples, Opts).





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
  html(
    span([class=Cs|Attrs], [
      html(a(href=Link, Content_0)),
      \zh_link_external(Term)
    ])
  ).
zh_link(_, _, _, _, Content_0, _) -->
  Content_0.


zh_link_query_term(C, Term1, QueryTerm) :-
  term_to_atom(Term1, Term2),
  QueryTerm =.. [C,Term2].


zh_link_external(Term) -->
  {rdf_is_iri(Term)}, !,
  html([" ",\external_link_icon(Term)]).
zh_link_external(_) --> [].
