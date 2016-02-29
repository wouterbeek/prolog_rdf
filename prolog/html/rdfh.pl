:- module(
  rdfh,
  [
    rdfh_alias//1,         % +Alias
    rdfh_bnode//1,         % +B
    rdfh_class//1,         % +C
    rdfh_iri//1,           % +Iri
    rdfh_literal//1,       % +Lit
    rdfh_object//1,        % +O
    rdfh_objects//1,       % +Os
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

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(html/html_bs)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_list)).
:- use_module(library(html/html_meta)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(settings)).

:- setting(rdfh:handle_id, atom, root, '').



rdfh_alias(Alias) -->
  html(span(class=alias, Alias)).



rdfh_bnode(B) -->
  html(span(class=bnode, B)).



rdfh_class(C) -->
  html(span=class, \rdfh_iri(C)).



rdfh_iri(I) -->
  html(span(class=iri,\rdfh_iri0(I))).

rdfh_iri0(I) -->
  {rdfs_label(I, Lbl)},
  html(Lbl).
rdfh_iri0(I) -->
  {rdf_global_id(Alias:Local, I)}, !,
  html([\rdfh_alias(Alias),:,span(class=local,Local)]).
rdfh_iri0(I) -->
  html(I).



rdfh_literal(Lit) -->
  html(span(class=literal, \rdfh_literal0(Lit))).

rdfh_literal0(S@LTag) --> !,
  {rdf_equal(rdf:langString, D)},
  html(span([class=D,lang=LTag], S)).
rdfh_literal0(V^^D) -->
  {rdf_subdatatype_of(D, xsd:string)}, !,
  html(span(class=D,V)).
rdfh_literal0(V^^D) -->
  {rdf_subdatatype_of(D, xsd:integer)}, !,
  html(span(class=D,'~D'-[V])).
rdfh_literal0(V^^D) -->
  {(rdf_subdatatype_of(D, xsd:float) ; rdf_subdatatype_of(D, xsd:double))}, !,
  html(span(class=D,'~G'-[V])).
rdfh_literal0(V^^D) -->
  {rdf11:xsd_date_time_type(D)}, !,
  html_date_time(V).
rdfh_literal0(V^^D) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(V).


rdfh_object(O) --> html(span(class=object, \rdfh_object0(O))).

rdfh_object0(O) --> {rdf_is_iri(O)}, !, rdfh_iri(O).
rdfh_object0(O) --> {rdf_is_literal(O)}, !, rdfh_literal(O).
rdfh_object0(O) --> {rdf_is_bnode(O)}, !, rdfh_bnode(O).

rdfh_objects([]) --> [].
rdfh_objects([H|T]) --> html(div(\rdfh_object(H))), rdfh_objects(T).


rdfh_po_row(P-Os) -->
  html(tr([td(\rdfh_property(P)),td(\rdfh_objects(Os))])).


rdfh_po_table(L) -->
  html(
    table(class=[table,'table-striped'], [
      thead(tr([th('Key'),th('Value')])),
      tbody(\'html*'(rdfh_po_row, L)) %'
    ])
  ).


rdfh_predicate(P) --> html(span(class=predicate, \rdfh_iri(P))).


rdfh_property(Prop) --> html(span(class=property, \rdfh_iri(Prop))).

rdfh_property_path([]) --> [].
rdfh_property_path([H]) --> rdfh_property(H).
rdfh_property_path([H|T]) --> html([\rdfh_property(H),/,\rdfh_property_path(T)]).


rdfh_subject(S) --> html(span(class=subject, \rdfh_subject0(S))).

rdfh_subject0(S) --> {rdf_is_iri(S)}, !, rdfh_iri(S).
rdfh_subject0(S) --> {rdf_is_bnode(S)}, !, rdfh_bnode(S).


rdfh_term(T) --> html(span(class=term, \rdfh_term0(T))).

rdfh_term0(B) --> {rdf_is_bnode(B)}, !, rdfh_bnode(B).
rdfh_term0(L) --> {rdf_is_literal(L)}, !, rdfh_literal(L).
rdfh_term0(I) --> {rdf_is_iri(I)}, !, rdfh_iri(I).


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

common_link(I) -->
  html([a(href=I, I), \bs_link_icon]).


external_link(I) -->
  html(a(href=I,\bs_link_icon)).



internal_link(T, I) :-
  setting(rdfh:handle_id, HandleId),
  http_link_to_id(HandleId, [term(T)], I).
