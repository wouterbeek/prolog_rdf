:- module(
  rdfh,
  [
    rdfh_alias//1,         % +Alias
    rdfh_bnode//1,         % +B
    rdfh_class//1,         % +C
    rdfh_datatype//1,      % +D
    rdfh_graph//1,         % +G
    rdfh_iri//1,           % +Iri
    rdfh_literal//1,       % +Lit
    rdfh_object//1,        % +O
    rdfh_po_row//1,        % +Pair
    rdfh_po_table//1,      % +Pairs
    rdfh_predicate//1,     % +P
    rdfh_property//1,      % +Prop
    rdfh_property_path//1, % +Props
    rdfh_subject//1,       % +S
    rdfh_term//1,          % +T
    rdfh_tree//1,          % +Tree
    rdfh_triple//3         % +S, +P, +O
  ]
).

/** <module> RDF HTML

Generates end user-oriented HTML representations of RDF data.

This assumes that an HTTP handler with id `rdfh` is defined.

@author Wouter Beek
@version 2016/02-2016/03
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_list)).
:- use_module(library(html/html_meta)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(settings)).

:- html_meta
   rdfh_link(+, html, ?, ?).





rdfh_alias(Alias) --> html(Alias).



rdfh_bnode(B) --> rdfh_link(bnode(B)).



rdfh_class(C) --> rdfh_link(class(C)).



rdfh_datatype(D) --> rdfh_link(datatype(D)).



rdfh_graph(G) --> rdfh_link(graph(G)).



% Abbreviated notation for IRI.
rdfh_iri(Iri) -->
  {rdf_global_id(Alias:Local, Iri)}, !,
  html([\rdfh_alias(Alias),":",Local]).
% RDFS label replacing IRI or plain IRI.
rdfh_iri(Iri) -->
  {(rdfs_label(Iri, Lbl) -> true ; Lbl = Iri)},
  html(Lbl).



% RDF language-tagged string.
rdfh_literal(S@LTag) --> !,
  html(span(lang=LTag, S)).
% XSD string.
rdfh_literal(V^^D) -->
  {rdf_subdatatype_of(D, xsd:string)}, !,
  html(V).
% XSD integer.
rdfh_literal(V^^D) -->
  {rdf_subdatatype_of(D, xsd:integer)}, !,
  html('~D'-[V]).
% XSD float or double.
rdfh_literal(V^^D) -->
  {(  rdf_subdatatype_of(D, xsd:float)
  ;   rdf_subdatatype_of(D, xsd:double)
  )}, !,
  html('~G'-[V]).
% XSD date/time.
rdfh_literal(V^^D) -->
  {rdf11:xsd_date_time_type(D)}, !,
  html_date_time(V).
% XSD URI
rdfh_literal(V^^D) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(V).



% IRI.
rdfh_object(O) --> {rdf_is_iri(O)}, !, rdfh_iri(O).
% Literal.
rdfh_object(O) --> {rdf_is_literal(O)}, !, rdfh_literal(O).
% Blank node.
rdfh_object(O) --> {rdf_is_bnode(O)}, !, rdfh_bnode(O).



%! rdfh_po_row(+Pair)// is det.

rdfh_po_row(P-Os) -->
  html(tr([td(\rdfh_property(P)),td(\html_seplist(rdfh_object, Os))])).



rdfh_po_table(L) -->
  html(
    table(class=[table,'table-striped'], [
      thead(tr([th('Key'),th('Value')])),
      tbody(\html_maplist(rdfh_po_row, L))
    ])
  ).



rdfh_predicate(P) -->
  html(span(class=predicate, \rdfh_link(predicate(P)))).



rdfh_property(Prop) --> rdfh_link(property(Prop)).



rdfh_property_path(L) --> html_seplist(rdfh_property, L).



rdfh_subject(S) --> {rdf_is_iri(S)}, !, rdfh_iri(S).
rdfh_subject(S) --> {rdf_is_bnode(S)}, !, rdfh_bnode(S).



% Blank node
rdfh_term(B) --> {rdf_is_bnode(B)}, !, rdfh_bnode(B).
% Literal
rdfh_term(L) --> {rdf_is_literal(L)}, !, rdfh_literal(L).
% IRI
rdfh_term(I) --> {rdf_is_iri(I)}, !, rdfh_iri(I).



rdfh_tree(Tree) -->
  html([
    \check_all,
    div(class=treeview, div(class=tree,\rdfh_trees([0], [Tree])))
  ]).

rdfh_trees(_, []) --> !, [].
rdfh_trees(Ns, [P-[Leaf-[]]|Trees]) --> !,
  html([
    div(class=node, [\rdfh_predicate(P),' ',\rdfh_object(Leaf)]),
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



rdfh_triple(S, P, O) -->
  html([
    "〈",
    \rdfh_subject(S),
    ", ",
    \rdfh_predicate(P),
    ", ",
    \rdfh_object(O),
    "〉"
  ]).





% HELPERS %

rdfh_link(Query, Content_2) -->
  {http_link_to_id(rdfh, [Query], Uri)},
  internal_link(Uri, Content_2).
