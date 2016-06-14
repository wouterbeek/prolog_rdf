:- module(
  rdfh,
  [
    rdfh_bnode//1,         % +B
    rdfh_bnode//2,         % +B,             +Opts
    rdfh_class//1,         % +C
    rdfh_class//2,         % +C,             +Opts
    rdfh_datatype//1,      % +D
    rdfh_datatype//2,      % +D,             +Opts
    rdfh_describe//1,      % +S
    rdfh_graph//1,         % +G
    rdfh_graph_table//0,
    rdfh_iri//1,           % +Iri
    rdfh_list//1,          % +L
    rdfh_list//2,          % +L,             +Opts
    rdfh_literal//1,       % +Lit
    rdfh_literal//2,       % +Lit,           +Opts
    rdfh_object//1,        % +O
    rdfh_object//2,        % +O,             +Opts
    rdfh_predicate//1,     % +P
    rdfh_predicate//2,     % +P,             +Opts
    rdfh_property//1,      % +Prop
    rdfh_property//2,      % +Prop,          +Opts
    rdfh_property_path//1, % +Props
    rdfh_property_path//2, % +Props,         +Opts
    rdfh_quad//4,          % +S, +P, +O, +G
    rdfh_quad_panels//4,   % ?S, ?P, ?O, ?G
    rdfh_quad_table//4,    % ?S, ?P, ?O, ?G
    rdfh_subject//1,       % +S
    rdfh_subject//2,       % +S,             +Opts
    rdfh_term//1,          % +Term
    rdfh_term//2,          % +Term,          +Opts
    rdfh_tree//1,          % +Tree
    rdfh_triple//3,        % +S, +P, +O
    rdfh_triple//4,        % +S, +P, +O,     +Opts
    rdfh_triple_table//1,  % +Triples
    rdfh_triple_table//2,  % +Triples,       +Opts
    rdfh_triple_table//4,  % ?S, ?P, ?O, ?G
    rdfh_triple_table//5   % +S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> RDF HTML

Generates end user-oriented HTML representations of RDF data.

This assumes that an HTTP handler with id `rdfh` is defined.

The following options are supported:

  * max_length(+nonneg)

---

@author Wouter Beek
@version 2016/02-2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_bnode_map)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_prefix), []).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdfs/rdfs_ext)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(stat/rdf_stat)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).

:- dynamic
    rdf11:rdfh_literal_hook//2.

:- multifile
    rdf11:rdfh_literal_hook//2.

:- html_meta
   rdfh_link(+, +, +, html, +, ?, ?).

:- setting(rdfh_handler, atom, '',
     "ID of the HTTP handler that performs RDF term lookup."
   ).





%! rdfh_bnode(+BNode)// is det.
%! rdfh_bnode(+BNode, +Opts)// is det.

rdfh_bnode(B) -->
  {rdfh_default_options(Opts)},
  rdfh_bnode(B, Opts).

rdfh_bnode(B, Opts) -->
  rdfh_bnode_outer(_, [], B, Opts).

rdfh_bnode_outer(C, Cs1, B0, Opts) -->
  {
    ord_add_element(Cs1, bnode, Cs2),
    rdf_bnode_map(B0, B)
  },
  rdfh_link(C, Cs2, B, \rdfh_bnode_inner(B, Opts), Opts).

rdfh_bnode_inner(B, _) -->
  {rdf_bnode_map(B, Lbl)},
  html(["_:",Lbl]).



%! rdfh_class(+C)// is det.
%! rdfh_class(+Cs, +Opts)// is det.

rdfh_class(C) -->
  {rdfh_default_options(Opts)},
  rdfh_class(C, Opts).

rdfh_class(C, Opts) -->
  rdfh_class_outer(_, [], C, Opts).

rdfh_class_outer(C0, Cs1, C, Opts) -->
  {ord_add_element(Cs1, class, Cs2)},
  rdfh_term_outer(C0, Cs2, C, Opts).



%! rdfh_datatype(+D)// is det.
%! rdfh_datatype(+D, +Opts)// is det.

rdfh_datatype(D) -->
  {rdfh_default_options(Opts)},
  rdfh_datatype(D, Opts).

rdfh_datatype(D, Opts) -->
  rdfh_datatype_outer(_, [], D, Opts).

rdfh_datatype_outer(C, Cs1, D, Opts) -->
  {ord_add_element(Cs1, datatype, Cs2)},
  rdfh_term_outer(C, Cs2, D, Opts).



%! rdfh_describe(+S)// is det.
%! rdfh_describe(+S, +Opts)// is det.
%
% Generate a full description of subject term S.

rdfh_describe(S) -->
  {rdfh_default_options(Opts)},
  rdfh_describe(S, Opts).

rdfh_describe(S, Opts) -->
  {
    findall(P-O, rdf(S, P, O), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  bs_table(
    \bs_table_header(["Predicate","Objects"]),
    \html_maplist({Opts}/[Group]>>rdfh_describe_row(Group, Opts), Groups)
  ).

rdfh_describe_row(P-Os, Opts) -->
  html(
    tr([
      td(\rdfh_property_outer(property, [], P, Opts)),
      td(\html_seplist({Opts}/[O]>>rdfh_object(O, Opts), " ", Os))
    ])
  ).



%! rdfh_graph(+G)// is det.

rdfh_graph(G) -->
  {rdfh_default_options(Opts)},
  rdfh_graph(G, Opts).

rdfh_graph(G, Opts) -->
  rdfh_graph_outer(_, [], G, Opts).

rdfh_graph_outer(C, Cs1, G, Opts) -->
  {ord_add_element(Cs1, graph, Cs2)},
  rdfh_iri_outer(C, Cs2, G, Opts).



%! rdfh_graph_table// is det.
%! rdfh_graph_table(+Opts)// is det.

rdfh_graph_table -->
  {rdfh_default_options(Opts)},
  rdfh_graph_table(Opts).

rdfh_graph_table(Opts) -->
  {
    findall(NumTriples-G, rdf_number_of_triples(G, NumTriples), Pairs),
    desc_pairs_values(Pairs, Gs)
  },
  bs_table(
    \bs_table_header(["Graph","Number of triples"]),
    \html_maplist({Opts}/[G]>>rdfh_graph_row(G, Opts), Gs)
  ).

rdfh_graph_row(G, Opts) -->
  {rdf_number_of_triples(G, NumTriples)},
  html(
    tr([
      td(\rdfh_graph_outer(graph, [], G, Opts)),
      td(\html_thousands(NumTriples))
    ])
  ).



%! rdfh_iri(+Iri)// is det.
%! rdfh_iri(+Iri, +Opts)// is det.

rdfh_iri(Iri) -->
  {rdfh_default_options(Opts)},
  rdfh_iri(Iri, Opts).

rdfh_iri(Iri, Opts) -->
  rdfh_iri_outer(_, [], Iri, Opts).

rdfh_iri_outer(C, Cs1, Iri, Opts) -->
  {ord_add_element(Cs1, iri, Cs2)},
  rdfh_link(C, Cs2, Iri, \rdfh_iri_inner(Iri, Opts), Opts).

% Abbreviated notation for IRI.
rdfh_iri_inner(Iri, _) -->
  {rdf_global_id(Alias:Local, Iri)}, !,
  html([
    span(class=alias, Alias),
    ":",
    Local
  ]).
% RDFS label replacing IRI.
rdfh_iri_inner(Iri, _) -->
  {rdfs_pref_label(Iri, Lbl)}, !,
  html(Lbl).
% Plain IRI.
rdfh_iri_inner(Iri, _) -->
  html(Iri).



%! rdfh_list(+L)// is det.
%! rdfh_list(+L, +Opts)// is det.

rdfh_list(L) -->
  {rdfh_default_options(Opts)},
  rdfh_list(L, Opts).

rdfh_list(L, Opts) -->
  {rdf_list(L, Terms)},
  list({Opts}/[Term]>>rdfh_term(Term, Opts), Terms).



%! rdfh_literal(+Lit)// is det.
%! rdfh_literal(+Lit, +Opts)// is det.

rdfh_literal(Lit) -->
  {rdfh_default_options(Opts)},
  rdfh_literal(Lit, Opts).

rdfh_literal(Lit, Opts) -->
  rdfh_literal_outer(_, [], Lit, Opts).

rdfh_literal_outer(C, Cs1, Lit, Opts) -->
  {
    ord_add_element(Cs1, literal, Cs2),
    rdf_literal_datatype(Lit, D)
  },
  rdfh_link(C, Cs2, [datatype=D], Lit, \rdfh_literal_inner(Lit, Opts), Opts).

% RDF HTML
rdfh_literal_inner(V^^D, _) -->
  {rdf_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
rdfh_literal_inner(S@LTag, Opts) -->
  {get_dict(show_flag, Opts, true)}, !,
  html([
    span(lang(LTag), \bs_truncated(S, Opts.max_length)),
    " ",
    \flag_icon(LTag)
  ]).
rdfh_literal_inner(S@LTag, Opts) --> !,
  html(span(lang(LTag), \bs_truncated(S, Opts.max_length))).
% XSD boolean
rdfh_literal_inner(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD gDay
rdfh_literal_inner(Da^^D, _) -->
  {rdf_equal(xsd:gDay, D)}, !,
  html("~d"-[Da]).
% XSD gMonth
rdfh_literal_inner(Mo^^D, _) -->
  {rdf_equal(xsd:gMonth, D)}, !,
  html("~d"-[Mo]).
% XSD gYear
rdfh_literal_inner(Y^^D, _) -->
  {rdf_equal(xsd:gYear, D)}, !,
  html("~d"-[Y]).
% XSD date
% XSD dateTime
% XSD gMonthYear
% XSD gYearMonth
rdfh_literal_inner(V^^D1, Opts) -->
  {
    rdf_subdatatype_of(D1, D2),
    rdf11:xsd_date_time_type(D2)
  }, !,
  html_date_time(V, Opts).
% XSD decimal
rdfh_literal_inner(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD double
% XSD float
rdfh_literal_inner(V^^D, _) -->
  {(  rdf_subdatatype_of(D, xsd:float)
  ;   rdf_subdatatype_of(D, xsd:double)
  )}, !,
  html("~G"-[V]).
% XSD integer
rdfh_literal_inner(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD string
rdfh_literal_inner(S^^D, Opts) -->
  {rdf_subdatatype_of(D, xsd:string)}, !,
  bs_truncated(S, Opts.max_length).
% XSD URI
rdfh_literal_inner(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(V).
% ‘Raw’ array
rdfh_literal_inner(Lit, Opts) -->
  rdf11:rdfh_literal_inner(Lit, Opts).
rdfh_literal_inner(V^^D, _) -->
  {gtrace}, %DEB
  html([p(V),p(D)]).



%! rdfh_object(+O)// is det.
%! rdfh_object(+O, +Opts)// is det.

rdfh_object(O) -->
  {rdfh_default_options(Opts)},
  rdfh_object(O, Opts).

rdfh_object(O, Opts) -->
  rdfh_object_outer(_, [], O, Opts).

rdfh_object_outer(C, Cs1, O, Opts) -->
  {ord_add_element(Cs1, object, Cs2)},
  rdfh_term_outer(C, Cs2, O, Opts).



%! rdfh_predicate(+P)// is det.
%! rdfh_predicate(+P, +Opts)// is det.

rdfh_predicate(P) -->
  {rdfh_default_options(Opts)},
  rdfh_predicate(P, Opts).

rdfh_predicate(P, Opts) -->
  rdfh_predicate_outer(_, [], P, Opts).

rdfh_predicate_outer(C, Cs1, P, Opts) -->
  {ord_add_element(Cs1, predicate, Cs2)},
  rdfh_iri_outer(C, Cs2, P, Opts).



%! rdfh_property(+Prop)// is det.
%! rdfh_property(+Prop, +Opts)// is det.

rdfh_property(Prop) -->
  {rdfh_default_options(Opts)},
  rdfh_property(Prop, Opts).

rdfh_property(Prop, Opts) -->
  rdfh_property_outer(_, [], Prop, Opts).

rdfh_property_outer(C, Cs1, Prop, Opts) -->
  {ord_add_element(Cs1, property, Cs2)},
  rdfh_term_outer(C, Cs2, Prop, Opts).



%! rdfh_property_path(+Props       )// is det.
%! rdfh_property_path(+Props, +Opts)// is det.

rdfh_property_path(Props) -->
  {rdfh_default_options(Opts)},
  rdfh_property_path(Props, Opts).

rdfh_property_path(Props, Opts) -->
  html_seplist({Opts}/[Prop]>>rdfh_property(Prop, Opts), " ", Props).



%! rdfh_quad(+S, +P, +O, +G)// is det.

rdfh_quad(S, P, O, G) -->
  {rdfh_default_options(Opts)},
  rdfh_quad(S, P, O, G, Opts).

rdfh_quad(S, P, O, G, Opts) -->
  html(
    span(class=quadruple, [
      &(lang),
      \rdfh_triple_outer(_, S, P, O, Opts),
      ", ",
      \rdfh_graph_outer(graph, G, Opts),
      &(rang)
    ])
  ).



%! rdfh_quad_panels(?S, ?P, ?O, ?G)// is det.

rdfh_quad_panels(S, P, O, G) -->
  {
    findall(G-rdf(S,P,O), rdf(S, P, O, G), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  bs_panels(rdfh_triple_table, Groups).



%! rdfh_quad_row(+Quad)// is det.

rdfh_quad_row(Quad) -->
  {rdfh_default_options(Opts)},
  rdfh_quad_row(Quad, Opts).

rdfh_quad_row(rdf(S,P,O,G), Opts) -->
  html(
    span(class=quadruple,
      tr([
        td(\rdfh_subject_outer(subject, [], S, Opts)),
        td(\rdfh_predicate_outer(predicate, [], P, Opts)),
        td(\rdfh_object_outer(object, [], O, Opts)),
        td(\rdfh_graph_outer(graph, [], G, Opts))
      ])
    )
  ).



%! rdfh_quad_table(+Quads)// is det.
%! rdfh_quad_table(+Quads, +Opts)// is det.
%! rdfh_quad_table(?S, ?P, ?O, ?G)// is det.
%! rdfh_quad_table(?S, ?P, ?O, ?G, +Opts)// is det.

rdfh_quad_table(Quads) -->
  {rdfh_default_options(Opts)},
  rdfh_quad_table(Quads, Opts).

rdfh_quad_table(Quads, Opts) -->
  bs_table(
    \bs_table_header(["Subject","Predicate","Object","Graph"]),
    \html_maplist({Opts}/[Quad]>>rdfh_quad_row(Quad, Opts), Quads)
  ).

rdfh_quad_table(S, P, O, G) -->
  {rdfh_default_options(Opts)},
  rdfh_quad_table(S, P, O, G, Opts).

rdfh_quad_table(S, P, O, G, Opts) -->
  {findall(rdf(S,P,O,G), rdf(S, P, O, G), Quads)},
  rdfh_quad_table(Quads, Opts).



%! rdfh_subject(+S)// is det.
%! rdfh_subject(+S, +Opts)// is det.

rdfh_subject(S) -->
  {rdfh_default_options(Opts)},
  rdfh_subject(S, Opts).

rdfh_subject(S, Opts) -->
  rdfh_subject_outer(_, [], S, Opts).

rdfh_subject_outer(C, Cs1, S, Opts) -->
  {ord_add_element(Cs1, subject, Cs2)},
  (   {rdf_is_iri(S)}
  ->  rdfh_iri_outer(C, Cs2, S, Opts)
  ;   {rdf_is_bnode(S)}
  ->  rdfh_bnode_outer(C, Cs2, S, Opts)
  ).



%! rdfh_term(+Term)// is det.
%! rdfh_term(+Term, +Opts)// is det.

rdfh_term(Term) -->
  {rdfh_default_options(Opts)},
  rdfh_term(Term, Opts).

rdfh_term(Term, Opts) -->
  rdfh_term_outer(_, [], Term, Opts).

rdfh_term_outer(C, Cs1, Term, Opts) -->
  {ord_add_element(Cs1, term, Cs2)},
  (   {rdf_is_literal(Term)}
  ->  rdfh_literal_outer(C, Cs2, Term, Opts)
  ;   {rdf_is_bnode(Term)}
  ->  rdfh_bnode_outer(C, Cs2, Term, Opts)
  ;   {rdf_is_iri(Term)}
  ->  rdfh_iri_outer(C, Cs2, Term, Opts)
  ).



%! rdfh_tree(+Tree)// is det.

rdfh_tree(Tree) -->
  html([
    \check_all,
    div(class(treeview), div(class(tree),\rdfh_trees([0], [Tree])))
  ]).

check_all -->
  html([
    \js_script({|javascript(_)||
$("#checkAll").change(function () {
  $("input:checkbox").prop('checked', $(this).prop("checked"));
});
    |}),
    p(label([input([id(checkAll),type(checkbox)], []), 'Check all']))
  ]).

rdfh_trees(_, []) --> !, [].
rdfh_trees(Ns, [P-[Leaf-[]]|Trees]) --> !,
  html([
    div(class=node, [\rdfh_predicate_outer(P)," ",\rdfh_object_outer(Leaf)]),
    \rdfh_trees(Ns, Trees)
  ]).
rdfh_trees(Ns, [Leaf-[]|Trees]) --> !,
  html([
    div(class=node, \rdfh_object_outer(Leaf)),
    \rdfh_trees(Ns, Trees)
  ]).
rdfh_trees(Ns1, [Root-Subtrees|Trees]) -->
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
      label(for=Id, \rdfh_predicate_outer(Root)),
      div(class=tree, \rdfh_trees(Ns3, Subtrees))
    ]),
    \rdfh_trees(Ns2, Trees)
  ]).



%! rdfh_triple(+Triple)// is det.
%! rdfh_triple(+Triple, +Opts)// is det.
%! rdfh_triple(+S, +P, +O)// is det.
%! rdfh_triple(+S, +P, +O, +Opts)// is det.

rdfh_triple(Triple) -->
  {rdfh_default_options(Opts)},
  rdfh_triple(Triple, Opts).

rdfh_triple(rdf(S,P,O), Opts) -->
  rdfh_triple(S, P, O, Opts).

rdfh_triple(S, P, O) -->
  {rdfh_default_options(Opts)},
  rdfh_triple(S, P, O, Opts).

rdfh_triple(S, P, O, Opts) -->
  html(
    span(class=triple, [
      &(lang),
      \rdfh_subject_outer(subject, [], S, Opts),
      ", ",
      \rdfh_predicate_outer(predicate, [], P, Opts),
      ", ",
      \rdfh_object_outer(object, [], O, Opts),
      &(rang)
    ])
  ).



%! rdfh_triple_row(+Triple)// is det.
%! rdfh_triple_row(+Triple, +Opts)// is det.

rdfh_triple_row(Triple) -->
  {rdfh_default_options(Opts)},
  rdfh_triple_row(Triple, Opts).


rdfh_triple_row(rdf(S,P,O), Opts) -->
  html(
    span(class=triple,
      tr([
        td(\rdfh_subject_outer(subject, [], S, Opts)),
        td(\rdfh_predicate_outer(predicate, [], P, Opts)),
        td(\rdfh_object_outer(object, [], O, Opts))
      ])
    )
  ).



%! rdfh_triple_table(+Triples)// is det.
%! rdfh_triple_table(+Triples, +Opts)// is det.

rdfh_triple_table(Triples) -->
  {rdfh_default_options(Opts)},
  rdfh_triple_table(Triples, Opts).

rdfh_triple_table(Triples, Opts) -->
  bs_table(
    \rdfh_table_header,
    \html_maplist({Opts}/[Triple]>>rdfh_triple_row(Triple, Opts), Triples)
  ).

rdfh_table_header -->
  html(
    tr([
      th(class=subject, "Subject"),
      th(class=predicate, "Predicate"),
      th(class=object, "Object")
    ])
  ).



%! rdfh_triple_table(?S, ?P, ?O, ?G)// is det.
%! rdfh_triple_table(?S, ?P, ?O, ?G, +Opts)// is det.

rdfh_triple_table(S, P, O, G) -->
  {rdfh_default_options(Opts)},
  rdfh_triple_table(S, P, O, G, Opts).


rdfh_triple_table(S, P, O, G, Opts) -->
  {findall(rdf(S,P,O), rdf(S, P, O, G), Triples)},
  rdfh_triple_table(Triples, Opts).





% HELPERS %

rdfh_default_options(_{max_length: 50}).



%! rdfh_link(+C, +Cs, +Term, :Content_2, +Opts)// is det.
%! rdfh_link(+C, +Cs, +Attrs, +Term, :Content_2, +Opts)// is det.
%
% Generates an RDF request link in case HTTP handler `rdfh` is
% defined.  Otherwise, the content is generated without an enclosing
% link element.
%
% Cs is a list of classes.  Possible values are `datatype`, `graph`,
% `predicate`, `object`, `subject`, `term`.
%
% C is the class that denotes the context in which Term is displayed.

rdfh_link(C, Cs, Term, Content_2, Opts) -->
  rdfh_link(C, Cs, [], Term, Content_2, Opts).


rdfh_link(C, Cs, Attrs, Term, Content_2, Opts) -->
  {
    setting(rdfh_handler, Id),
    Id \== '', !,
    rdfh_link_query_term(C, Term, QueryTerm),
    (   get_dict(query, Opts, Query0)
    ->  Query = [QueryTerm|Query0]
    ;   Query = [QueryTerm]
    ),
    http_link_to_id(Id, Query, Link)
  },
  html(
    span([class=Cs|Attrs], [
      \internal_link(Link, Content_2),
      \rdfh_link_external(Term)
    ])
  ).
rdfh_link(_, _, _, _, Content_2, _) -->
  Content_2.

rdfh_link_query_term(C, Term1, QueryTerm) :-
  term_to_atom(Term1, Term2),
  QueryTerm =.. [C,Term2].

rdfh_link_external(Term) -->
  {rdf_is_iri(Term)}, !,
  html([" ",\external_link_icon(Term)]).
rdfh_link_external(_) --> [].
