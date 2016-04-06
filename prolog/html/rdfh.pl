:- module(
  rdfh,
  [
    rdfh_alias//1,         % +Alias
    rdfh_bnode//1,         % +B
    rdfh_bnode//2,         % +Opts, +B
    rdfh_class//1,         % +C
    rdfh_class//2,         % +Opts, +C
    rdfh_datatype//1,      % +D
    rdfh_datatype//2,      % +Opts, +D
    rdfh_describe//1,      % +S
    rdfh_graph//1,         % +G
    rdfh_iri//1,           % +Iri
    rdfh_list//1,          % +L
    rdfh_list//2,          % +Opts, +L
    rdfh_literal//1,       % +Lit
    rdfh_literal//2,       % +Opts, +Lit
    rdfh_object//1,        % +O
    rdfh_object//2,        % +Opts, +O
    rdfh_predicate//1,     % +P
    rdfh_predicate//2,     % +Opts, +P
    rdfh_property//1,      % +Prop
    rdfh_property//2,      % +Opts, +Prop
    rdfh_property_path//1, % +Props
    rdfh_property_path//2, % +Opts, +Props
    rdfh_quad//4,          % +S, P, +O, +G
    rdfh_quad_panels//4,   % ?S, ?P, ?O, ?G
    rdfh_quad_table//4,    % ?S, ?P, ?O, ?G
    rdfh_string//1,        % +Lit
    rdfh_subject//1,       % +S
    rdfh_subject//2,       % +Opts, +S
    rdfh_term//1,          % +Term
    rdfh_term//2,          % +Opts, +Term
    rdfh_tree//1,          % +Tree
    rdfh_triple//3,        % +S, +P, +O
    rdfh_triple//4,        % +Opts, +S, +P, +O
    rdfh_triple_table//1,  % +Triples
    rdfh_triple_table//2,  % +Opts, +Triples
    rdfh_triple_table//4,  % ?S, ?P, ?O, ?G
    rdfh_triple_table//5   % +Opts, ?S, ?P, ?O, ?G
  ]
).

/** <module> RDF HTML

Generates end user-oriented HTML representations of RDF data.

This assumes that an HTTP handler with id `rdfh` is defined.

@author Wouter Beek
@version 2016/02-2016/04
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(rdf/rdf_prefix), []).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(typecheck)).

:- html_meta
   rdfh_link(+, html, ?, ?),
   rdfh_link(+, +, html, ?, ?).

:- setting(rdfh_handler, atom, '',
     "ID of the HTTP handler that performs RDF term lookup."
   ).





%! rdfh_alias(+Alias)// is det.

rdfh_alias(Alias) -->
  html(Alias).



%! rdfh_bnode(+BNode)// is det.
%! rdfh_bnode(+Opts, +BNode)// is det.

rdfh_bnode(B) -->
  rdfh_bnode(_{}, B).

rdfh_bnode(Opts, B) -->
  rdfh_link(Opts, bnode=B, html(B)).



%! rdfh_class(+C)// is det.
%! rdfh_class(+Opts, +C)// is det.

rdfh_class(C) -->
  rdfh_class(_{}, C).

rdfh_class(Opts, C) -->
  rdfh_link(Opts, class=C, \rdfh_iri(C)).



%! rdfh_datatype(+D)// is det.
%! rdfh_datatype(+Opts, +D)// is det.

rdfh_datatype(D) -->
  rdfh_datatype(_{}, D).

rdfh_datatype(Opts, D) -->
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



%! rdfh_list(+List)// is det.
%! rdfh_list(+Opts, +List)// is det.

rdfh_list(L) -->
  rdfh_list(_{}, L).

rdfh_list(Opts, L) -->
  {rdf_list(L, Terms)},
  list(rdfh_term0(Opts), Terms).
rdfh_term0(Opts, Term) --> rdfh_term(Term, Opts).



%! rdfh_literal(+Lit)// is det.
%! rdfh_literal(+Opts, +Lit)// is det.

rdfh_literal(Lit) -->
  rdfh_literal(_{}, Lit).

rdfh_literal(Opts, Lit) -->
  rdfh_link(Opts, literal=Lit, \rdfh_literal0(Lit)).

% RDF HTML
rdfh_literal0(V^^D) -->
  {rdf_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string.
rdfh_literal0(S@LTag) --> !,
  html([span(lang=LTag, S)," ",\flag_icon(LTag)]).
% XSD boolean.
rdfh_literal0(V^^D) -->
  {rdf_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD date/time.
rdfh_literal0(V^^D1) -->
  {
    rdf_subdatatype_of(D1, D2),
    rdf11:xsd_date_time_type(D2)
  }, !,
  html_date_time(V).
% XSD decimal
rdfh_literal0(V^^D) -->
  {rdf_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD float & XSD double.
rdfh_literal0(V^^D) -->
  {(  rdf_subdatatype_of(D, xsd:float)
  ;   rdf_subdatatype_of(D, xsd:double)
  )}, !,
  html("~G"-[V]).
% XSD integer.
rdfh_literal0(V^^D) -->
  {rdf_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD string.
rdfh_literal0(V^^D) -->
  {rdf_subdatatype_of(D, xsd:string)}, !,
  html(V).
% XSD URI
rdfh_literal0(V^^D) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(V).



%! rdfh_object(+O)// is det.
%! rdfh_object(+Opts, +O)// is det.

rdfh_object(O) -->
  rdfh_object(_{}, O).

rdfh_object(Opts, O) -->
  rdfh_link(Opts, object=O, \rdfh_term0(O)).



%! rdfh_predicate(+P)// is det.
%! rdfh_predicate(+Opts, +P)// is det.

rdfh_predicate(P) -->
  rdfh_predicate(_{}, P).

rdfh_predicate(Opts, P) -->
  rdfh_link(Opts, predicate=P, \rdfh_iri(P)).



%! rdfh_property(+Prop)// is det.
%! rdfh_property(+Opts, +Prop)// is det.

rdfh_property(Prop) -->
  rdfh_property(_{}, Prop).

rdfh_property(Opts, Prop) -->
  rdfh_link(Opts, property=Prop, \rdfh_iri(Prop)).



%! rdfh_property_path(+Props)// is det.
%! rdfh_property_path(+Opts, +Props)// is det.

rdfh_property_path(Props) -->
  rdfh_property_path(_{}, Props).

rdfh_property_path(Opts, Props) -->
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



%! rdfh_subject(+S)// is det.
%! rdfh_subject(+Opts, +S)// is det.

rdfh_subject(S) -->
  rdfh_subject(_{}, S).

rdfh_subject(Opts, S) -->
  rdfh_link(Opts, subject=S, \rdfh_subject0(S)).

rdfh_subject0(S) -->
  {rdf_is_iri(S)}, !,
  rdfh_iri(S).
rdfh_subject0(S) -->
  {rdf_is_bnode(S)}, !,
  rdfh_bnode(S).



%! rdfh_term(+T)// is det.
%! rdfh_term(+Opts, +T)// is det.

rdfh_term(T) -->
  rdfh_term(_{}, T).

rdfh_term(Opts, T) -->
  rdfh_link(Opts, term=T, \rdfh_term0(T)).

rdfh_term0(Lit) -->
  {rdf_is_literal(Lit)}, !,
  rdfh_literal0(Lit).
rdfh_term0(S) -->
  rdfh_subject0(S).


%! rdfh_tree(+Tree)// is det.

rdfh_tree(Tree) -->
  html([
    \check_all,
    div(class=treeview, div(class=tree,\rdfh_trees([0], [Tree])))
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
%! rdfh_triple(+S, +P, +O)// is det.
%! rdfh_triple(+Opts, +S, +P, +O)// is det.

rdfh_triple(rdf(S,P,O)) -->
  html(div, \rdfh_triple(S, P, O)).


rdfh_triple(S, P, O) -->
  rdfh_triple(_{}, S, P, O).


rdfh_triple(Opts, S, P, O) -->
  html([
    &(lang),
    \rdfh_subject(Opts, S),
    ", ",
    \rdfh_predicate(Opts, P),
    ", ",
    \rdfh_object(Opts, O),
    &(rang)
  ]).



%! rdfh_triple_row(+Triple)// is det.
%! rdfh_triple_row(+Opts, +Triple)// is det.

rdfh_triple_row(Triple) -->
  rdfh_triple_row(_{}, Triple).

rdfh_triple_row(Opts, rdf(S,P,O)) -->
  html(
    tr([
      td(\rdfh_subject(Opts, S)),
      td(\rdfh_predicate(Opts, P)),
      td(\rdfh_object(Opts, O))
    ])
  ).



%! rdfh_triple_table(+Trips)// is det.
%! rdfh_triple_table(+Opts, +Trips)// is det.

rdfh_triple_table(L) -->
  rdfh_triple_table(_{}, L).

rdfh_triple_table(Opts, L) -->
  bs_table(\rdfh_table_header, \html_maplist(rdfh_triple_row(Opts), L)).

rdfh_table_header -->
  html(
    tr([
      th(class=subject, "Subject"),
      th(class=predicate, "Predicate"),
      th(class=object, "Object")
    ])
  ).



%! rdfh_triple_table(?S, ?P, ?O, ?G)// is det.
%! rdfh_triple_table(+Opts, ?S, ?P, ?O, ?G)// is det.

rdfh_triple_table(S, P, O, G) -->
  rdfh_triple_table(_{}, S, P, O, G).

rdfh_triple_table(Opts, S, P, O, G) -->
  {findall(rdf(S,P,O), rdf(S, P, O, G), L)},
  rdfh_triple_table(Opts, L).





% HELPERS %

/*
html_entry(owl:equivalentClass, equiv).
html_entry(owl:sameAs,          equiv).
html_entry(rdf:type,            isin).
html_entry(rdfs:subClassOf,     sube).
*/

%! rdfh_link(+Query, :Content_2)// is det.
%! rdfh_link(+Opts, +Query, :Content_2)// is det.
% Generates an RDF request link in case HTTP handler `rdfh` is defined.
% Otherwise, the content is generated without an enclosing link element.

rdfh_link(Query, Content_2) -->
  rdfh_link(_{}, Query, Content_2).

rdfh_link(Opts, N=V1, Content_2) -->
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
rdfh_link(_, _, Content_2) -->
  Content_2.
