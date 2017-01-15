:- module(
  rdf_html,
  [
    rdf_html_alias//1,         %     +Alias
    rdf_html_bnode//1,         %     +BNode
    rdf_html_class//1,         %     +C
    rdf_html_class//2,         %     +C,             +Opts
    rdf_html_dataset_term//1,  %     +D
    rdf_html_dataset_term//2,  %     +D,             +Opts
    rdf_html_describe//3,      % +M, +S, +G
    rdf_html_describe//4,      % +M, +S, +G          +Opts
    rdf_html_graph_table//1,   %                     +Opts
    rdf_html_graph_term//1,    %     +G
    rdf_html_graph_term//2,    %     +G,             +Opts
    rdf_html_instance//1,      %     +I
    rdf_html_instance//2,      %     +I,             +Opts
    rdf_html_iri//1,           %     +Iri
    rdf_html_iri//2,           %     +Iri,           +Opts
    rdf_html_literal//1,       %     +Lit
    rdf_html_literal//2,       %     +Lit,           +Opts
    rdf_html_object//1,        %     +O
    rdf_html_object//2,        %     +O,             +Opts
    rdf_html_p_os_table//1,    %     +Pairs
    rdf_html_p_os_table//2,    %     +Pairs,         +Opts
    rdf_html_predicate//1,     %     +P
    rdf_html_predicate//2,     %     +P,             +Opts
    rdf_html_property//1,      %     +Prop
    rdf_html_property//2,      %     +Prop,          +Opts
    rdf_html_property_path//1, %     +Props
    rdf_html_property_path//2, %     +Props,         +Opts
    rdf_html_quad//4,          %     +S, +P, +O, +G
    rdf_html_quad//5,          %     +S, +P, +O, +G, +Opts
    rdf_html_quad_panels//5,   % +M, ?S, ?P, ?O, ?G
    rdf_html_quad_panels//6,   % +M, ?S, ?P, ?O, ?G, +Opts
    rdf_html_subject//1,       %     +S
    rdf_html_subject//2,       %     +S,             +Opts
    rdf_html_term//1,          %     +Term
    rdf_html_term//2,          %     +Term,          +Opts
    rdf_html_tree//1,          %     +Tree
    rdf_html_tree//2,          %     +Tree,          +Opts
    rdf_html_triple//1,        %     +Triple
    rdf_html_triple//2,        %     +Triple,        +Opts
    rdf_html_triple//3,        %     +S, +P, +O
    rdf_html_triple//4,        %     +S, +P, +O,     +Opts
    rdf_html_triple_table//1,  %     +Triples
    rdf_html_triple_table//2,  %     +Triples,       +Opts
    rdf_html_triple_table//5,  % +M, ?S, ?P, ?O, ?G
    rdf_html_triple_table//6   % +M, ?S, ?P, ?O, ?G, +Opts
  ]
).

/** <module> Quine HTML basics

Generates end user-oriented HTML representations of RDF data.

The following options are supported to achieve parity with module
`rdf_print`:

  - iri_abbr(+boolean)

  - max_iri_len(+or([nonneg,oneof([inf])]))

  - max_lit_len(+or([nonneg,oneof([inf])]))

The following options are specific to this module:

  - show_flag(+boolean)

---

@author Wouter Beek
@version 2016/02-2017/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_stat)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf11), [rdf_meta/1]).
:- use_module(library(string_ext)).
:- use_module(library(tree/s_tree)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).

:- multifile
    html:html_hook//1,
    html:html_hook//2,
    rdf_html_literal_hook//2,
    rdf11:in_ground_type_hook/3,
    rdf11:out_type_hook/3.

:- rdf_meta
   rdf_html_class(r, ?, ?),
   rdf_html_class(r, +, ?, ?),
   rdf_html_dataset_term(r, ?, ?),
   rdf_html_dataset_term(r, +, ?, ?),
   rdf_html_describe(+, r, r, ?, ?),
   rdf_html_describe(+, r, r, +, ?, ?),
   rdf_html_graph_term(r, ?, ?),
   rdf_html_graph_term(r, +, ?, ?),
   rdf_html_instance(r, ?, ?),
   rdf_html_instance(r, +, ?, ?),
   rdf_html_iri(r, ?, ?),
   rdf_html_iri(r, +, ?, ?),
   rdf_html_literal(o, ?, ?),
   rdf_html_literal(o, +, ?, ?),
   rdf_html_object(o, ?, ?),
   rdf_html_object(o, +, ?, ?),
   rdf_html_predicate(r, ?, ?),
   rdf_html_predicate(r, +, ?, ?),
   rdf_html_property(r, ?, ?),
   rdf_html_property(r, +, ?, ?),
   rdf_html_property_path(t, ?, ?),
   rdf_html_property_path(t, +, ?, ?),
   rdf_html_quad(r, r, o, r, ?, ?),
   rdf_html_quad(r, r, o, r, +, ?, ?),
   rdf_html_quad_panels(+, r, r, o, r, ?, ?),
   rdf_html_quad_panels(+, r, r, o, r, +, ?, ?),
   rdf_html_subject(r, ?, ?),
   rdf_html_subject(r, +, ?, ?),
   rdf_html_term(o, ?, ?),
   rdf_html_term(o, +, ?, ?),
   rdf_html_triple(r, r, o, ?, ?),
   rdf_html_triple(r, r, o, +, ?, ?),
   rdf_html_triple_table(+, r, r, o, r, ?, ?),
   rdf_html_triple_table(+, r, r, o, r, +, ?, ?).

html:html_hook(Opts, q_dataset_term(D)) -->
  rdf_html_dataset_term(D, Opts).
html:html_hook(Opts, q_graph_term(G)) -->
  rdf_html_graph_term(G, Opts).
html:html_hook(Opts, q_iri(Iri)) -->
  rdf_html_iri(Iri, Opts).





%! rdf_html_alias(+Alias)// is det.

rdf_html_alias(Alias) -->
  html(Alias).



%! rdf_html_bnode(+BNode)// is det.

rdf_html_bnode(BNode) -->
  {q_is_bnode(BNode)}, !,
  html(BNode).
rdf_html_bnode(BNode) -->
  {q_bnode_label(BNode, Lbl)}, !,
  html(["_:",Lbl]).



%! rdf_html_class(+C)// is det.
%! rdf_html_class(+C, +Opts)// is det.

rdf_html_class(C) -->
  rdf_html_class(C, _{}).


rdf_html_class(C, Opts) -->
  rdf_html_term(C, Opts).



%! rdf_html_dataset_term(+D)// is det.
%! rdf_html_dataset_term(+D, +Opts)// is det.

rdf_html_dataset_term(D) -->
  rdf_html_dataset_term(D, _{}).


rdf_html_dataset_term(D, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_dataset_term0(D, Opts2).

rdf_html_dataset_term0(D, Opts) -->
  rdf_html_term0(D, Opts).



%! rdf_html_describe(+M, +S, +G)// is det.
%! rdf_html_describe(+M, +S, +G, +Opts)// is det.
%
% Generate a full description of subject term S.
%
% @tbd Align with (S)CBD.

rdf_html_describe(M, S, G) -->
  rdf_html_describe(M, S, G, _{}).


rdf_html_describe(M, S, G, Opts) -->
  {
    findall(P-O, t(M, S, P, O, G), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  rdf_html_p_os_table(Groups, Opts).



%! rdf_html_graph_table(+Opts)// is det.

rdf_html_graph_table(Opts1) -->
  {
    rdf_graph_table_comps(HeaderRow, DataRows),
    rdf_html_default_table_options(Opts1, Opts2)
  },
  table_content(html_hook(Opts2), [head(HeaderRow)|DataRows]).



%! rdf_html_graph_term(+G)// is det.
%! rdf_html_graph_term(+G, +Opts)// is det.

rdf_html_graph_term(G) -->
  rdf_html_graph_term(G, _{}).


rdf_html_graph_term(G, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_graph_term0(G, Opts2).

rdf_html_graph_term0(G, Opts) -->
  rdf_html_term0(G, Opts).



%! rdf_html_instance(+I)// is det.
%! rdf_html_instance(+I, +Opts)// is det.

rdf_html_instance(I) -->
  rdf_html_instance(I, _{}).


rdf_html_instance(I, Opts) -->
  rdf_html_term(I, Opts).



%! rdf_html_iri(+Iri)// is det.
%! rdf_html_iri(+Iri, +Opts)// is det.

rdf_html_iri(Iri) -->
  rdf_html_iri(Iri, _{}).


rdf_html_iri(Iri, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_iri0(Iri, Opts2),
  rdf_html_iri_external0(Iri).

% Abbreviated notation for IRI.
rdf_html_iri0(Iri, _) -->
  {rdf_global_id(rdf:type, Iri)}, !,
  html("a").
rdf_html_iri0(Iri, Opts) -->
  {
    Opts.iri_abbr == true,
    rdf_global_id(Alias:Local1, Iri), !,
    atom_ellipsis(Local1, Opts.max_iri_len, Local2)
  },
  html([Alias,":",Local2]).
% Plain IRI, possibly ellipsed.
rdf_html_iri0(Iri1, Opts) -->
  {atom_ellipsis(Iri1, Opts.max_iri_len, Iri2)},
  html(Iri2).

rdf_html_iri_external0(Iri) -->
  {is_http_iri(Iri)}, !,
  html([" ",\external_link_icon(Iri)]).
rdf_html_iri_external0(_) --> [].



%! rdf_html_literal(+Lit)// is det.
%! rdf_html_literal(+Lit, +Opts)// is det.

rdf_html_literal(Lit) -->
  rdf_html_literal(Lit, _{}).


rdf_html_literal(Lit, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_literal0(Lit, Opts2).

rdf_html_literal0(Lit, Opts) -->
  rdf_html_literal00(Lit, Opts),
  rdf_html_literal_external0(Lit).

% RDF HTML
rdf_html_literal00(V^^D, _) -->
  {rdf_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
rdf_html_literal00(Str@LTag, Opts) -->
  {get_dict(show_flag, Opts, true)}, !,
  html([
    span(lang(LTag), \ellipsis(Str, Opts.max_lit_len)),
    " ",
    \flag_icon(LTag)
  ]).
rdf_html_literal00(Str@LTag, Opts) --> !,
  html(span(lang=LTag, \ellipsis(Str, Opts.max_lit_len))).
% XSD boolean
rdf_html_literal00(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD gDay
rdf_html_literal00(Da^^D, _) -->
  {rdf_equal(xsd:gDay, D)}, !,
  html("~d"-[Da]).
% XSD gMonth
rdf_html_literal00(Mo^^D, _) -->
  {rdf_equal(xsd:gMonth, D)}, !,
  html("~d"-[Mo]).
% XSD gYear
rdf_html_literal00(Y^^D, _) -->
  {rdf_equal(xsd:gYear, D)}, !,
  html("~d"-[Y]).
% XSD integer
rdf_html_literal00(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD decimal
rdf_html_literal00(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD double
% XSD float
rdf_html_literal00(V^^D, _) -->
  {(rdf_subdatatype_of(D, xsd:float) ; rdf_subdatatype_of(D, xsd:double))}, !,
  html("~G"-[V]).
% XSD string
rdf_html_literal00(Str^^D, Opts) -->
  {rdf_subdatatype_of(D, xsd:string)}, !,
  ellipsis(Str, Opts.max_lit_len).
% XSD URI
rdf_html_literal00(Uri^^D, _) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(Uri).
% XSD date
% XSD dateTime
% XSD gMonthYear
% XSD gYearMonth
rdf_html_literal00(V^^D1, Opts) -->
  {
    % @bug here
    rdf11:xsd_date_time_type(D2),
    rdf_subdatatype_of(D1, D2)
  }, !,
  html_date_time(V, Opts).
% Datatype hooks.
rdf_html_literal00(Lit, Opts) -->
  rdf_html_literal_hook(Lit, Opts), !.
% Other literals for which there is no hook.
% E.g., http://www.opengis.net/ont/geosparql#wktLiteral
rdf_html_literal00(Lit, Opts) -->
  {rdf_literal_lexical_form(Lit, Lex)},
  html(\ellipsis(Lex, Opts.max_lit_len)).

rdf_html_literal_external0(Uri^^D) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(" "),
  {uri_components(Uri, uri_components(Scheme,_,_,_,_))},
  (   {Scheme == mailto}
  ->  mail_icon(Uri)
  ;   {memberchk(Scheme, [http,https])}
  ->  external_link_icon(Uri)
  ).
rdf_html_literal_external0(_) --> [].



%! rdf_html_object(+O)// is det.
%! rdf_html_object(+O, +Opts)// is det.

rdf_html_object(O) -->
  rdf_html_object(O, _{}).


rdf_html_object(O, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_object0(O, Opts2).


rdf_html_object0(O, Opts) -->
  rdf_html_term(O, Opts).



%! rdf_html_p_os_table(+Groups)// is det.
%! rdf_html_p_os_table(+Groups, +Opts)// is det.

rdf_html_p_os_table(Groups) -->
  rdf_html_p_os_table(Groups, _{}).


rdf_html_p_os_table(Groups, Opts1) -->
  {
    HeaderRow = ["Predicate","Objects"],
    rdf_html_default_table_options(Opts1, Opts2)
  },
  table(
    \table_header_row(HeaderRow),
    \html_maplist(
      {Opts2}/[Group]>>rdf_html_p_os_row0(Group, Opts2),
      Groups
    )
  ).


rdf_html_p_os_row0(Opts, P-Os) -->
  html(
    tr([
      td(\rdf_html_predicate(P, Opts)),
      td(
	\html_seplist(
	  {Opts}/[O]>>rdf_html_object(O, Opts),
	  " ",
	  Os
	)
      )
    ])
  ).



%! rdf_html_predicate(+P)// is det.
%! rdf_html_predicate(+P, +Opts)// is det.

rdf_html_predicate(P) -->
  rdf_html_predicate(P, _{}).


rdf_html_predicate(P, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_predicate0(P, Opts2).


rdf_html_predicate0(P, Opts) -->
  rdf_html_iri(P, Opts).



%! rdf_html_property(+Prop)// is det.
%! rdf_html_property(+Prop, +Opts)// is det.

rdf_html_property(Prop) -->
  rdf_html_property(Prop, _{}).


rdf_html_property(Prop, Opts) -->
  rdf_html_term(Prop, Opts).



%! rdf_html_property_path(+Props)// is det.
%! rdf_html_property_path(+Props, +Opts)// is det.

rdf_html_property_path(Props) -->
  rdf_html_property_path(Props, _{}).


rdf_html_property_path(Props, Opts) -->
  html_seplist({Opts}/[Prop]>>rdf_html_property(Prop, Opts), " ", Props).



%! rdf_html_quad(+S, +P, +O, +G)// is det.
%! rdf_html_quad(+S, +P, +O, +G, +Opts)// is det.

rdf_html_quad(S, P, O, G) -->
  rdf_html_quad(S, P, O, G, _{}).


rdf_html_quad(S, P, O, G, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_quad0(S, P, O, G, Opts2).

rdf_html_quad0(S, P, O, G, Opts) -->
  html(
    span([
      &(lang),
      \rdf_html_triple0(S, P, O, Opts),
      ", ",
      \rdf_html_graph_term0(G, Opts),
      &(rang)
    ])
  ).



%! rdf_html_quad_panels(+M, ?S, ?P, ?O, ?G)// is det.
%! rdf_html_quad_panels(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

rdf_html_quad_panels(M, S, P, O, G) -->
  rdf_html_quad_panels(M, S, P, O, G, _{}).


rdf_html_quad_panels(M, S, P, O, G, Opts1) -->
  {
    rdf_html_default_table_options(Opts1, Opts2),
    findall(G-Triple, rdf_triple(M, S, P, O, G, Triple), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  panels(
    html_maplist(
      {Opts2}/[Group]>>rdf_html_triple_table(Group, Opts2),
      Groups
    )
  ).



%! rdf_html_subject(+S)// is det.
%! rdf_html_subject(+S, +Opts)// is det.

rdf_html_subject(S) -->
  rdf_html_subject(S, _{}).


rdf_html_subject(S, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_subject0(S, Opts2).

rdf_html_subject0(BNode, _) -->
  rdf_html_bnode(BNode), !.
rdf_html_subject0(Iri, Opts) -->
  {rdf_is_iri(Iri)}, !,
  rdf_html_iri0(Iri, Opts).



%! rdf_html_term(+Term)// is det.
%! rdf_html_term(+Term, +Opts)// is det.

rdf_html_term(Term) -->
  rdf_html_term(Term, _{}).


rdf_html_term(Term, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_term0(Term, Opts2).

rdf_html_term0(Term, Opts) -->
  rdf_html_subject0(Term, Opts).
rdf_html_term0(Lit, Opts) -->
  {rdf_is_literal(Lit)}, !,
  rdf_html_literal0(Lit, Opts).



%! rdf_html_tree(+Tree)// is det.
%! rdf_html_tree(+Tree, +Opts)// is det.

rdf_html_tree(Tree) -->
  rdf_html_tree(Tree, _{}).


rdf_html_tree(Tree, Opts1) -->
  {rdf_html:rdf_html_default_options(Opts1, Opts2)},
  html([
    \check_all0,
    div(class(treeview), div(class(tree),\rdf_html_trees0([0], [Tree], Opts2)))
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

rdf_html_trees0(_, [], _) --> !, [].
rdf_html_trees0(Ns, [P-[Leaf-[]]|Trees], Opts) --> !,
  html([
    div(class=node, [
      \rdf_html_predicate(P, Opts),
      " ",
      \rdf_html_object(Leaf, Opts)
    ]),
    \rdf_html_trees0(Ns, Trees, Opts)
  ]).
rdf_html_trees0(Ns, [Leaf-[]|Trees], Opts) --> !,
  html([
    div(class=node, \rdf_html_object(Leaf, Opts)),
    \rdf_html_trees0(Ns, Trees, Opts)
  ]).
rdf_html_trees0(Ns1, [Root-Subtrees|Trees], Opts) -->
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
      label(for=Id, \rdf_html_predicate(Root, Opts)),
      div(class=tree, \rdf_html_trees0(Ns3, Subtrees, Opts))
    ]),
    \rdf_html_trees0(Ns2, Trees, Opts)
  ]).



%! rdf_html_triple(+Triple)// is det.
%! rdf_html_triple(+Triple, +Opts)// is det.
%! rdf_html_triple(+S, +P, +O)// is det.
%! rdf_html_triple(+S, +P, +O, +Opts)// is det.

rdf_html_triple(Triple) -->
  rdf_html_triple(Triple, _{}).


rdf_html_triple(rdf(S,P,O), Opts) -->
  rdf_html_triple(S, P, O, Opts).


rdf_html_triple(S, P, O) -->
  rdf_html_triple(S, P, O, _{}).


rdf_html_triple(S, P, O, Opts1) -->
  {rdf_html_default_options(Opts1, Opts2)},
  rdf_html_triple0(S, P, O, Opts2).

rdf_html_triple0(S, P, O, Opts) -->
  html(
    span([
      &(lang),
      \rdf_html_subject0(S, Opts),
      ", ",
      \rdf_html_predicate0(P, Opts),
      ", ",
      \rdf_html_object0(O, Opts),
      &(rang)
    ])
  ).



%! rdf_html_triple_table(+Triples)// is det.
%! rdf_html_triple_table(+Triples, +Opts)// is det.

rdf_html_triple_table(Triples) -->
  rdf_html_triple_table(Triples, _{}).


rdf_html_triple_table(Triples, Opts1) -->
  {
    HeaderRow = ["Subject","Predicate","Object"],
    rdf_html_default_table_options(Opts1, Opts2)
  },
  table(
    \table_header_row(HeaderRow),
    \html_maplist(
      {Opts2}/[Triple]>>rdf_html_triple_row0(Triple, Opts2),
      Triples
    )
  ).

rdf_html_triple_row0(rdf(S,P,O), Opts) -->
  html(
    tr(class=triple, [
      td(class='col-md-4',
        \rdf_html_subject(S, Opts.put(_{query_key: subject}))
      ),
      td(class='col-md-3',
        \rdf_html_predicate(P, Opts.put(_{query_key: predicate}))
      ),
      td(class='col-md-5',
        \rdf_html_object(O, Opts.put(_{query_key: object}))
      )
    ])
  ).



%! rdf_html_triple_table(+M, ?S, ?P, ?O, ?G)// is det.
%! rdf_html_triple_table(+M, ?S, ?P, ?O, ?G, +Opts)// is det.

rdf_html_triple_table(M, S, P, O, G) -->
  rdf_html_triple_table(M, S, P, O, G, _{}).


rdf_html_triple_table(M, S, P, O, G, Opts1) -->
  {
    rdf_html_default_table_options(Opts1, Opts2),
    rdf_triples(M, S, P, O, G, Triples)
  },
  rdf_html_triple_table(Triples, Opts2).





% HELPERS %

%! rdf_html_default_options(+Opts1, -Opts2) is det.

rdf_html_default_options(Opts1, Opts2) :-
  DefOpts = _{iri_abbr: true, max_iri_len: 30, max_lit_len: 75},
  merge_dicts(DefOpts, Opts1, Opts2).



%! rdf_html_default_table_options(-Opts) is det.
%! rdf_html_default_table_options(+Opts1, -Opts2) is det.

rdf_html_default_table_options(Opts) :-
  rdf_html_default_table_options(_{}, Opts).


rdf_html_default_table_options(Opts1, Opts2) :-
  rdf_html:rdf_html_default_options(_{max_iri_len: 25}, DefOpts),
  merge_dicts(DefOpts, Opts1, Opts2).
