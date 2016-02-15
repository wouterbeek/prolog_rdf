:- module(
  rdfh,
  [
    rdfh_alias//1,     % +Alias
    rdfh_bnode//1,     % +B
    rdfh_iri//1,       % +Iri
    rdfh_literal//1,   % +Lit
    rdfh_object//1,    % +O
    rdfh_po_table//1,  % +Pairs
    rdfh_predicate//1, % +P
    rdfh_property//1,  % +Prop
    rdfh_subject//1    % +S
  ]
).

/** <module> RDF HTML

Generates end user-oriented HTML representations of RDF data.

@author Wouter Beek
@version 2016/02
*/

:- use_module(library(html/html_datetime)).
:- use_module(library(html/html_meta)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(settings)).

:- setting(rdfh:handle_id, atom, root, '').

:- rdf_meta
   rdfh_iri(r, ?, ?),
   rdfh_literal(o, ?, ?),
   rdfh_object(o, ?, ?),
   rdfh_predicate(r, ?, ?),
   rdfh_property(r, ?, ?),
   rdfh_subject(r, ?, ?).



rdfh_alias(Alias) -->
  {rdf_current_prefix(Alias, Prefix)},
  html(a([class=alias,href=Prefix], Alias)).



rdfh_bnode(B) -->
  html(span(class=bnode, B)).



rdfh_iri(I) -->
  {internal_link(I, I0)},
  html(a([class=iri,href=I0], [\rdfh_iri0(I),' ',\external_link(I)])).

rdfh_iri0(I) -->
  {rdfs_label(I, Lbl)}, !,
  html([\rdfh_literal(Lbl)]).
rdfh_iri0(I) -->
  {rdf_global_id(Alias:Local, I)}, !,
  html([span(class=alias,Alias),:,span(class=local,Local)]).
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
rdfh_literal0(V1^^D) -->
  {rdf11:xsd_date_time_type(D)}, !,
  {date_time_datetime(V1, V2)},
  html_datetime(V2).



rdfh_object(O) --> html(span(class=object, \rdfh_object0(O))).

rdfh_object0(O) --> {rdf_is_iri(O)}, !, rdfh_iri(O).
rdfh_object0(O) --> {rdf_is_literal(O)}, !, rdfh_literal(O).
rdfh_object0(O) --> {rdf_is_bnode(O)}, !, rdfh_bnode(O).



rdfh_po_table(L) -->
  html(
    table(class=[table,'table-striped'], [
      thead(tr([th('Key'),th('Value')])),
      tbody(\'*'(rdfh_po_row, L))
    ])
  ). %'

rdfh_po_row(P-O) -->
  html(tr([td(\rdfh_predicate(P)),td(\rdfh_object(O))])).



rdfh_predicate(P) --> html(span(class=predicate, \rdfh_iri(P))).



rdfh_property(Prop) --> html(span(class=property, \rdfh_iri(Prop))).



rdfh_subject(S) --> html(span(class=subject, \rdfh_subject0(S))).

rdfh_subject0(S) --> {rdf_is_iri(S)}, !, rdfh_iri(S).
rdfh_subject0(S) --> {rdf_is_bnode(S)}, !, rdfh_bnode(S).





% HELPERS %

'*'([]) --> [].
'*'([H|T]) --> html_call(H), '*'(T).


'*'(_, []) --> [].
'*'(Goal, [H|T]) --> html_call(Goal, H), '*'(Goal, T).



date_time_datetime(date_time(Y,Mo,D,H,Mi,S), datetime(Y,Mo,D,H,Mi,S,0)).



external_link(I) -->
  html(a(href=I,span(['aria-hidden'=true,class=[glyphicon,'glyphicon-link']],[]))).



internal_link(T, I) :-
  setting(handle_id, HandleId),
  http_link_to_id(HandleId, [term(T)], I).
