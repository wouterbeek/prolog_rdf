:- module(
  rdfh,
  [
    rdfh_alias//1,         % +Alias
    rdfh_bnode//1,         % +B
    rdfh_bnode//2,         % +B,             +Opts
    rdfh_class//1,         % +C	            
    rdfh_class//2,         % +C,             +Opts
    rdfh_datatype//1,      % +D	            
    rdfh_datatype//2,      % +D,             +Opts
    rdfh_describe//1,      % +S	            
    rdfh_graph//1,         % +G	            
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
    rdfh_string//1,        % +Lit
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

@author Wouter Beek
@version 2016/02-2016/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_prefix), []).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(typecheck)).
:- use_module(library(yall)).

:- html_meta
   rdfh_link(html, +, ?, ?),
   rdfh_link(+, html, +, ?, ?).

:- setting(rdfh_handler, atom, '',
     "ID of the HTTP handler that performs RDF term lookup."
   ).





%! rdfh_alias(+Alias)// is det.

rdfh_alias(Alias) -->
  html(Alias).



%! rdfh_bnode(+BNode       )// is det.
%! rdfh_bnode(+BNode, +Opts)// is det.

rdfh_bnode(B) -->
  rdfh_bnode(B, _{}).


rdfh_bnode(B, Opts) -->
  rdfh_link(bnode=B, html(B), Opts).



%! rdfh_class(+C       )// is det.
%! rdfh_class(+C, +Opts)// is det.

rdfh_class(C) -->
  rdfh_class(C, _{}).


rdfh_class(Opts, C) -->
  rdfh_link(Opts, class=C, \rdfh_iri(C)).



%! rdfh_datatype(+D       )// is det.
%! rdfh_datatype(+D, +Opts)// is det.

rdfh_datatype(D) -->
  rdfh_datatype(D, _{}).


rdfh_datatype(D, Opts) -->
  rdfh_link(Opts, datatype=D, \rdfh_iri(D)).



%! rdfh_describe(+S)// is det.
% Generate a full description of subject term S.

rdfh_describe(S) -->
  {
    findall(P-O, rdf(S, P, O), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  bs_table(
    \bs_table_header(["Predicate","Objects"]),
    \html_maplist(rdfh_describe_row, Groups)
  ).

rdfh_describe_row(P-Os) -->
  html(tr([td(\rdfh_property(P)),td(\html_seplist(rdfh_object, Os))])).



%! rdfh_graph(+G)// is det.

rdfh_graph(G) -->
  rdfh_link(graph=G, \rdfh_iri(G)).



%! rdfh_iri(+Iri)// is det.

% Abbreviated notation for IRI.
rdfh_iri(Iri) -->
  {rdf_global_id(Alias:Local, Iri)}, !,
  html([\rdfh_alias(Alias),":",Local]).
% RDFS label replacing IRI.
rdfh_iri(Iri) -->
  {rdfs_label(Iri, Lbl)}, !,
  html(Lbl).
% Plain IRI.
rdfh_iri(Iri) -->
  html(Iri).



%! rdfh_list(+List       )// is det.
%! rdfh_list(+List, +Opts)// is det.

rdfh_list(L) -->
  rdfh_list(L, _{}).


rdfh_list(L, Opts) -->
  {rdf_list(L, Terms)},
  list(rdfh_term0(Opts), Terms).



%! rdfh_literal(+Lit       )// is det.
%! rdfh_literal(+Lit, +Opts)// is det.

rdfh_literal(Lit) -->
  rdfh_literal(Lit, _{}).


rdfh_literal(Lit, Opts) -->
  rdfh_link(literal=Lit, rdfh_literal0(Lit, Opts), Opts).

% RDF HTML
rdfh_literal0(V^^D, _) -->
  {rdf_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string.
rdfh_literal0(S@LTag, Opts) -->
  {get_dict(show_flag, Opts, true)}, !,
  html([span(lang=LTag, S)," ",\flag_icon(LTag)]).
rdfh_literal0(S@LTag, _) --> !,
  html(span(lang=LTag, S)).
% XSD boolean.
rdfh_literal0(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD date/time.
rdfh_literal0(V^^D1, _) -->
  {
    rdf_subdatatype_of(D1, D2),
    rdf11:xsd_date_time_type(D2)
  }, !,
  html_date_time(V).
% XSD decimal
rdfh_literal0(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD float & XSD double.
rdfh_literal0(V^^D, _) -->
  {(  rdf_subdatatype_of(D, xsd:float)
  ;   rdf_subdatatype_of(D, xsd:double)
  )}, !,
  html("~G"-[V]).
% XSD integer.
rdfh_literal0(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD string.
rdfh_literal0(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:string)}, !,
  html(V).
% XSD URI
rdfh_literal0(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(V).



%! rdfh_object(+O       )// is det.
%! rdfh_object(+O, +Opts)// is det.

rdfh_object(O) -->
  rdfh_object(O, _{}).


rdfh_object(O, Opts) -->
  rdfh_link(Opts, object=O, \rdfh_term0(O, Opts)).



%! rdfh_predicate(+P       )// is det.
%! rdfh_predicate(+P, +Opts)// is det.

rdfh_predicate(P) -->
  rdfh_predicate(P, _{}).


rdfh_predicate(P, Opts) -->
  rdfh_link(Opts, predicate=P, \rdfh_iri(P)).



%! rdfh_property(+Prop       )// is det.
%! rdfh_property(+Prop, +Opts)// is det.

rdfh_property(Prop) -->
  rdfh_property(Prop, _{}).


rdfh_property(Prop, Opts) -->
  rdfh_link(Opts, property=Prop, \rdfh_iri(Prop)).



%! rdfh_property_path(+Props       )// is det.
%! rdfh_property_path(+Props, +Opts)// is det.

rdfh_property_path(Props) -->
  rdfh_property_path(Props, _{}).


rdfh_property_path(Props, Opts) -->
  html_seplist(rdfh_property(Opts), Props).



%! rdfh_quad(+S, +P, +O, +G)// is det.

rdfh_quad(S, P, O, G) -->
  html([&(lang),\rdfh_triple(S, P, O),", ",\rdfh_graph(G),&(rang)]).



%! rdfh_quad_panels(?S, ?P, ?O, ?G)// is det.

rdfh_quad_panels(S, P, O, G) -->
  {
    findall(G-rdf(S,P,O), rdf(S, P, O, G), Pairs),
    group_pairs_by_key(Pairs, Groups)
  },
  bs_panels(rdfh_triple_table, Groups).



%! rdfh_quad_row(+Quad)// is det.

rdfh_quad_row(rdf(S,P,O,G)) -->
  html(
    tr([
      td(\rdfh_subject(S)),
      td(\rdfh_predicate(P)),
      td(\rdfh_object(O)),
      td(\rdfh_graph(G))
    ])
  ).



%! rdfh_quad_table(+Quads)// is det.

rdfh_quad_table(Quads) -->
  bs_table(
    \bs_table_header(["Subject","Predicate","Object","Graph"]),
    \html_maplist(rdfh_quad_row, Quads)
  ).


%! rdfh_quad_table(?S, ?P, ?O, ?G)// is det.

rdfh_quad_table(S, P, O, G) -->
  {findall(rdf(S,P,O,G), rdf(S, P, O, G), L)},
  rdfh_quad_table(L).



%! rdfh_string(+Lit)// is det.

rdfh_string(Lit) -->
  {rdf_string(Lit, String)},
  html(String).



%! rdfh_subject(+S       )// is det.
%! rdfh_subject(+S, +Opts)// is det.

rdfh_subject(S) -->
  rdfh_subject(S, _{}).


rdfh_subject(S, Opts) -->
  rdfh_link(Opts, subject=S, \rdfh_subject0(S)).

rdfh_subject0(S) -->
  {rdf_is_iri(S)}, !,
  rdfh_iri(S).
rdfh_subject0(S) -->
  {rdf_is_bnode(S)}, !,
  rdfh_bnode(S).



%! rdfh_term(+Term       )// is det.
%! rdfh_term(+Term, +Opts)// is det.

rdfh_term(Term) -->
  rdfh_term(Term, _{}).


rdfh_term(Term, Opts) -->
  rdfh_link(Opts, term=Term, \rdfh_term0(Term, Opts)).

rdfh_term0(Lit, Opts) -->
  {rdf_is_literal(Lit)}, !,
  rdfh_literal0(Lit, Opts).
rdfh_term0(S, _) -->
  rdfh_subject0(S).



%! rdfh_tree(+Tree)// is det.

rdfh_tree(Tree) -->
  html([
    \check_all,
    div(class=treeview, div(class=tree,\rdfh_trees([0], [Tree])))
  ]).

check_all -->
  html([
    \js_script({|javascript(_)||
$("#checkAll").change(function () {
  $("input:checkbox").prop('checked', $(this).prop("checked"));
});
    |}),
    p(label([input([id=checkAll,type=checkbox], []), 'Check all']))
  ]).

rdfh_trees(_, []) --> !, [].
rdfh_trees(Ns, [P-[Leaf-[]]|Trees]) --> !,
  html([
    div(class=node, [\rdfh_predicate(P)," ",\rdfh_object(Leaf)]),
    \rdfh_trees(Ns, Trees)
  ]).
rdfh_trees(Ns, [Leaf-[]|Trees]) --> !,
  html([
    div(class=node, \rdfh_object(Leaf)),
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
      label(for=Id, \rdfh_predicate(Root)),
      div(class=tree, \rdfh_trees(Ns3, Subtrees))
    ]),
    \rdfh_trees(Ns2, Trees)
  ]).



%! rdfh_triple(+Triple)// is det.
%! rdfh_triple(+S, +P, +O       )// is det.
%! rdfh_triple(+S, +P, +O, +Opts)// is det.

rdfh_triple(rdf(S,P,O)) -->
  html(div, \rdfh_triple(S, P, O)).


rdfh_triple(S, P, O) -->
  rdfh_triple(S, P, O, _{}).


rdfh_triple(S, P, O, Opts) -->
  html([
    &(lang),
    \rdfh_subject(S, Opts),
    ", ",
    \rdfh_predicate(P, Opts),
    ", ",
    \rdfh_object(O, Opts),
    &(rang)
  ]).



%! rdfh_triple_row(+Triple       )// is det.
%! rdfh_triple_row(+Triple, +Opts)// is det.

rdfh_triple_row(Triple) -->
  rdfh_triple_row(Triple, _{}).


rdfh_triple_row(rdf(S,P,O), Opts) -->
  html(
    tr([
      td(\rdfh_subject(S, Opts)),
      td(\rdfh_predicate(P, Opts)),
      td(\rdfh_object(O, Opts))
    ])
  ).



%! rdfh_triple_table(+Triples       )// is det.
%! rdfh_triple_table(+Triples, +Opts)// is det.

rdfh_triple_table(Triples) -->
  rdfh_triple_table(Triples, _{}).


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



%! rdfh_triple_table(?S, ?P, ?O, ?G       )// is det.
%! rdfh_triple_table(?S, ?P, ?O, ?G, +Opts)// is det.

rdfh_triple_table(S, P, O, G) -->
  rdfh_triple_table(S, P, O, G, _{}).


rdfh_triple_table(S, P, O, G, Opts) -->
  {findall(rdf(S,P,O), rdf(S, P, O, G), L)},
  rdfh_triple_table(L, Opts).





% HELPERS %

/*
html_entry(owl:equivalentClass, equiv).
html_entry(owl:sameAs,          equiv).
html_entry(rdf:type,            isin).
html_entry(rdfs:subClassOf,     sube).
*/

%! rdfh_link(+Query, :Content_2       )// is det.
%! rdfh_link(+Query, :Content_2, +Opts)// is det.
% Generates an RDF request link in case HTTP handler `rdfh` is defined.
% Otherwise, the content is generated without an enclosing link element.

rdfh_link(Query, Content_2) -->
  rdfh_link(Query, Content_2, _{}).


rdfh_link(N=V1, Content_2, Opts) -->
  {
    setting(rdfh_handler, Id),
    Id \== '',
    rdf_global_term(V1, V2),
    (is_iri(V2) -> Iri = true ; Iri = false),
    term_to_atom(V2, V3),
    (   get_dict(query, Opts, Query1)
    ->  Query2 = [N=V3|Query1]
    ;   Query2 = [N=V3]
    ),
    http_link_to_id(Id, Query2, Location), !
  },
  internal_link(Location, Content_2),
  ({Iri == true} -> html([" ",\external_link_icon(V3)]) ; "").
rdfh_link(_, Content_2, _) -->
  Content_2.
