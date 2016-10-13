:- module(
  qh,
  [
    qh_alias//1,         % +Alias
    qh_alias//2,         % +Alias,         +Opts
    qh_bnode//1,         % +B
    qh_bnode//2,         % +B,             +Opts
    qh_class//1,         % +C
    qh_class//2,         % +C,             +Opts
    qh_dataset_term//1,  % +D
    qh_dataset_term//2,  % +D,             +Opts
    qh_datatype//1,      % +D
    qh_datatype//2,      % +D,             +Opts
    qh_graph_term//1,    % +G
    qh_graph_term//2,    % +G,             +Opts
    qh_iri//1,           % +Iri
    qh_iri//2,           % +Iri,           +Opts
    qh_link//4,          % +C, +Cs, +Term, :Content_0
    qh_literal//1,       % +Lit
    qh_literal//2,       % +Lit,           +Opts
    qh_object//1,        % +O
    qh_object//2,        % +O,             +Opts
    qh_predicate//1,     % +P
    qh_predicate//2,     % +P,             +Opts
    qh_property//1,      % +Prop
    qh_property//2,      % +Prop,          +Opts
    qh_property_path//1, % +Props
    qh_property_path//2, % +Props,         +Opts
    qh_quad//4,          % +S, +P, +O, +G
    qh_quad//5,          % +S, +P, +O, +G, +Opts
    qh_subject//1,       % +S
    qh_subject//2,       % +S,             +Opts
    qh_term//1,          % +Term
    qh_term//2,          % +Term,          +Opts
    qh_triple//3,        % +S, +P, +O
    qh_triple//4,        % +S, +P, +O,     +Opts
  % -------------------------
    qh_dataset_term_outer0//4, % +Class, +Classes, +Opts, +D
    qh_default_options/2,      % +Opts1, -Opts2
    qh_graph_term_outer0//4,   % +Class, +Classes, +Opts, +G
    qh_object_outer0//4,       % +Class, +Classes, +Opts, +O
    qh_predicate_outer0//4,    % +Class, +Classes, +Opts, +P
    qh_property_outer0//4,     % +Class, +Classes, +Opts, +P
    qh_subject_outer0//4       % +Class, +Classes, +Opts, +S
  ]
).

/** <module> Quine HTML basics

Generates end user-oriented HTML representations of RDF data.

This assumes that an HTTP handler with id `qh` is defined.

The following option sets the HTTP handler:

  - handle_id(+atom)

The following options are supported to achieve parity with module
`q_print`:

  - iri_abbr(+boolean)

  - max_iri_len(+or([nonneg,oneof([inf])]))

  - max_lit_len(+or([nonneg,oneof([inf])]))

---

@tbd Achieve parity with option `bnode_map` from module `q_print`.

@author Wouter Beek
@version 2016/02-2016/09
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_bnode_map)).
:- use_module(library(q/q_dataset)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_fs)).
:- use_module(library(q/q_graph)).
:- use_module(library(q/q_rdf)).
:- use_module(library(q/q_rdfs)).
:- use_module(library(q/q_stat)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(settings)).
:- use_module(library(typecheck)).

:- multifile
    html:html_hook//1,
    html:html_hook//2,
    qh_literal_hook//2,
    rdf11:in_ground_type_hook/3,
    rdf11:out_type_hook/3.

:- rdf_meta
   qh_something(o, ?, ?),
   qh_something(o, +, ?, ?),
   qh_class(r, ?, ?),
   qh_class(r, +, ?, ?),
   qh_dataset_term(r, ?, ?),
   qh_dataset_term(r, +, ?, ?),
   qh_datatype(r, ?, ?),
   qh_datatype(r, +, ?, ?),
   qh_graph_term(r, ?, ?),
   qh_graph_term(r, +, ?, ?),
   qh_iri(r, ?, ?),
   qh_iri(r, +, ?, ?),
   qh_literal(o, ?, ?),
   qh_literal(o, +, ?, ?),
   qh_object(o, ?, ?),
   qh_object(o, +, ?, ?),
   qh_predicate(r, ?, ?),
   qh_predicate(r, +, ?, ?),
   qh_property(r, ?, ?),
   qh_property(r, +, ?, ?),
   qh_property_path(t, ?, ?),
   qh_property_path(t, +, ?, ?),
   qh_quad(r, r, o, r, ?, ?),
   qh_quad(r, r, o, r, +, ?, ?),
   qh_subject(r, ?, ?),
   qh_subject(r, +, ?, ?),
   qh_term(o, ?, ?),
   qh_term(o, +, ?, ?),
   qh_triple(r, r, o, ?, ?),
   qh_triple(r, r, o, +, ?, ?).

:- setting(
     handle_id,
     atom,
     '',
     "The default HTTP handler ID for performs RDF term lookup."
   ).

html:html_hook(Opts, q_alias(Alias)) -->
  qh_alias(Alias, Opts).
html:html_hook(Opts, q_dataset_term(D)) -->
  qh_dataset_term(D, Opts).
html:html_hook(Opts, q_graph_term(G)) -->
  qh_graph_term(G, Opts).
html:html_hook(Opts, q_iri(Iri)) -->
  qh_iri(Iri, Opts).





%! qh_alias(+Alias)// is det.
%! qh_alias(+Alias, +Opts)// is det.

qh_alias(Alias) -->
  qh_alias(Alias, _{}).


qh_alias(Alias, Opts) -->
  {
    qh_external_iri(alias, Alias, Link, Opts),
    q_alias_prefix(Alias, Prefix)
  },
  html([
    a([class=alias,href=Link], Alias),
    \qh_link_external(Prefix)
  ]).



%! qh_bnode(+B)// is det.
%! qh_bnode(+B, +Opts)// is det.

qh_bnode(B) -->
  qh_bnode(B, _{}).


qh_bnode(B, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_bnode_outer0(_, [], Opts2, B).


qh_bnode_outer0(C, Cs1, Opts, B) -->
  {
    ord_add_element(Cs1, bnode, Cs2),
    q_bnode_map(B, Lbl)
  },
  qh_link(C, Cs2, B, qh_bnode_inner(Lbl, Opts), Opts).


qh_bnode_inner(B, _) -->
  {q_bnode_map(B, Lbl)},
  html(["_:",Lbl]).



%! qh_class(+C)// is det.
%! qh_class(+Cs, +Opts)// is det.

qh_class(C) -->
  qh_class(C, _{}).


qh_class(C, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_class_outer0(_, [], Opts2, C).


qh_class_outer0(C0, Cs1, Opts, C) -->
  {ord_add_element(Cs1, class, Cs2)},
  qh_term_outer0(C0, Cs2, Opts, C).



%! qh_dataset_term(+D)// is det.
%! qh_dataset_term(+D, +Opts)// is det.

qh_dataset_term(D) -->
  qh_dataset_term(D, _{}).


qh_dataset_term(D, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_dataset_term_outer0(dataset, [], Opts2, D).


qh_dataset_term_outer0(C, Cs1, Opts, D) -->
  {ord_add_element(Cs1, dataset, Cs2)},
  qh_link(C, Cs2, [dataset=D], D, qh_dataset_term_inner(D, Opts), Opts).


qh_dataset_term_inner(D, _) -->
  {
    q_dataset_default_graph(D, DefG),
    q_pref_label(hdt, D, Lit, DefG),
    q_literal_string(Lit, Str)
  }, !,
  html(Str).
qh_dataset_term_inner(D, Opts) -->
  qh_iri_inner(D, Opts).



%! qh_datatype(+D)// is det.
%! qh_datatype(+D, +Opts)// is det.

qh_datatype(D) -->
  qh_datatype(D, _{}).


qh_datatype(D, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_datatype_outer0(_, [], Opts2, D).


qh_datatype_outer0(C, Cs1, Opts, D) -->
  {ord_add_element(Cs1, datatype, Cs2)},
  qh_term_outer0(C, Cs2, Opts, D).



%! qh_graph_term(+G)// is det.
%! qh_graph_term(+G, +Opts)// is det.

qh_graph_term(G) -->
  qh_graph_term(G, _{}).


qh_graph_term(G, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_graph_term_outer0(graph, [], Opts2, G).


qh_graph_term_outer0(C, Cs1, Opts, G) -->
  {ord_add_element(Cs1, graph, Cs2)},
  qh_link(C, Cs2, [graph=G], G, qh_graph_term_inner(G, Opts), Opts).


qh_graph_term_inner(G, Opts) -->
  {
    q_dataset_graph(D, G),
    q_dataset_default_graph(D, DefG),
    q_pref_label(hdt, G, Lit, DefG),
    q_literal_string(Lit, Str)
  }, !,
  html([\qh_dataset_term_inner(D, Opts),"/",Str]).
qh_graph_term_inner(G, Opts) -->
  qh_iri_inner(G, Opts).



%! qh_iri(+Iri)// is det.
%! qh_iri(+Iri, +Opts)// is det.

qh_iri(Iri) -->
  qh_iri(Iri, _{}).


qh_iri(Iri, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_iri_outer0(term, [term], Opts2, Iri).


qh_iri_outer0(C, Cs1, Opts, Iri) -->
  {ord_add_element(Cs1, iri, Cs2)},
  qh_link(C, Cs2, Iri, qh_iri_inner(Iri, Opts), Opts).


% Abbreviated notation for IRI.
qh_iri_inner(Iri, Opts) -->
  {
    Opts.iri_abbr == true,
    rdf_global_id(Alias:Local1, Iri), !,
    atom_ellipsis(Local1, Opts.max_iri_len, Local2)
  },
  html([span(class=alias, Alias),":",Local2]).
% Plain IRI, possibly ellipsed.
qh_iri_inner(Iri1, Opts) -->
  {atom_ellipsis(Iri1, Opts.max_iri_len, Iri2)},
  html(Iri2).



%! qh_literal(+Lit)// is det.
%! qh_literal(+Lit, +Opts)// is det.

qh_literal(Lit) -->
  qh_literal(Lit, _{}).


qh_literal(Lit, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_literal_outer0(literal, [], Opts2, Lit).


qh_literal_outer0(C, Cs1, Opts, Lit) -->
  {
    ord_add_element(Cs1, literal, Cs2),
    q_literal_datatype(Lit, D)
  },
  qh_link(C, Cs2, [datatype=D], Lit, qh_literal_inner(Lit, Opts), Opts).


% RDF HTML
qh_literal_inner(V^^D, _) -->
  {q_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
qh_literal_inner(Str@LTag, Opts) -->
  {get_dict(show_flag, Opts, true)}, !,
  html([
    span(lang(LTag), \ellipsis(Str, Opts.max_lit_len)),
    " ",
    \flag_icon(LTag)
  ]).
qh_literal_inner(Str@LTag, Opts) --> !,
  html(span(lang=LTag, \ellipsis(Str, Opts.max_lit_len))).
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
  ellipsis(Str, Opts.max_lit_len).
% XSD URI
qh_literal_inner(V^^D, _) -->
  {q_subdatatype_of(D, xsd:anyURI)}, !,
  html(V).
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
% Datatype hooks.
qh_literal_inner(Lit, Opts) -->
  qh_literal_hook(Lit, Opts).



%! qh_object(+O)// is det.
%! qh_object(+O, +Opts)// is det.

qh_object(O) -->
  qh_object(O, _{}).


qh_object(O, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_object_outer0(_, [], Opts2, O).


qh_object_outer0(C, Cs1, Opts, O) -->
  {ord_add_element(Cs1, object, Cs2)},
  qh_term_outer0(C, Cs2, Opts, O).



%! qh_predicate(+P)// is det.
%! qh_predicate(+P, +Opts)// is det.

qh_predicate(P) -->
  qh_predicate(P, _{}).


qh_predicate(P, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_predicate_outer0(_, [], Opts2, P).


qh_predicate_outer0(C, Cs1, Opts, P) -->
  {ord_add_element(Cs1, predicate, Cs2)},
  qh_iri_outer0(C, Cs2, Opts, P).



%! qh_property(+Prop)// is det.
%! qh_property(+Prop, +Opts)// is det.

qh_property(Prop) -->
  qh_property(Prop, _{}).


qh_property(Prop, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_property_outer0(property, [property], Opts2, Prop).


qh_property_outer0(C, Cs1, Opts, Prop) -->
  {ord_add_element(Cs1, property, Cs2)},
  qh_term_outer0(C, Cs2, Opts, Prop).



%! qh_property_path(+Props)// is det.
%! qh_property_path(+Props, +Opts)// is det.

qh_property_path(Props) -->
  qh_property_path(Props, _{}).


qh_property_path(Props, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  html_seplist(
    qh_property_outer0(property, [property], Opts2),
    " ",
    Props
  ).



%! qh_quad(+S, +P, +O, +G)// is det.
%! qh_quad(+S, +P, +O, +G, +Opts)// is det.

qh_quad(S, P, O, G) -->
  qh_quad(S, P, O, G, _{}).


qh_quad(S, P, O, G, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  html(
    span(class=quadruple, [
      &(lang),
      \qh_triple(S, P, O, Opts2),
      ", ",
      \qh_graph_term_outer0(graph, [graph], Opts2, G),
      &(rang)
    ])
  ).



%! qh_subject(+S)// is det.
%! qh_subject(+S, +Opts)// is det.

qh_subject(S) -->
  qh_subject(S, _{}).


qh_subject(S, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_subject_outer0(_, [], Opts2, S).


qh_subject_outer0(C, Cs1, Opts, S) -->
  {ord_add_element(Cs1, subject, Cs2)},
  (   {q_is_iri(S)}
  ->  qh_iri_outer0(C, Cs2, Opts, S)
  ;   {q_is_bnode(S)}
  ->  qh_bnode_outer0(C, Cs2, Opts, S)
  ).



%! qh_term(+Term)// is det.
%! qh_term(+Term, +Opts)// is det.

qh_term(Term) -->
  qh_term(Term, _{}).


qh_term(Term, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_term_outer0(term, [], Opts2, Term).


qh_term_outer0(C, Cs1, Opts, Term) -->
  {ord_add_element(Cs1, term, Cs2)},
  (   {q_is_literal(Term)}
  ->  qh_literal_outer0(C, Cs2, Opts, Term)
  ;   {q_is_bnode(Term)}
  ->  qh_bnode_outer0(C, Cs2, Opts, Term)
  ;   {q_is_iri(Term)}
  ->  qh_iri_outer0(C, Cs2, Opts, Term)
  ).



%! qh_triple(+Triple)// is det.
%! qh_triple(+Triple, +Opts)// is det.
%! qh_triple(+S, +P, +O)// is det.
%! qh_triple(+S, +P, +O, +Opts)// is det.

qh_triple(Triple) -->
  qh_triple(Triple, _{}).


qh_triple(rdf(S,P,O), Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_triple(S, P, O, Opts2).


qh_triple(S, P, O) -->
  qh_triple(S, P, O, _{}).


qh_triple(S, P, O, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  html(
    span(class=triple, [
      &(lang),
      \qh_subject_outer0(term, [subject], Opts2, S),
      ", ",
      \qh_predicate_outer0(predicate, [predicate], Opts2, P),
      ", ",
      \qh_object_outer0(term, [object], Opts2, O),
      &(rang)
    ])
  ).





% HELPERS %

%! qh_default_options(+Opts1, -Opts2) is det.

qh_default_options(Opts1, Opts2) :-
  DefOpts = _{
    iri_abbr: true,
    max_iri_len: 50,
    max_lit_len: inf,
    qh_link: false
  },
  merge_dicts(DefOpts, Opts1, Opts2).



%! qh_external_iri(+C, +Val, -Link) is det.
%! qh_external_iri(+C, +Val, -Link, +Opts) is det.

qh_external_iri(C, Val, Link) :-
  qh_external_iri(C, Val, Link, _{}).


qh_external_iri(C, Val, Link, Opts) :-
  dict_get(query, Opts, [], Query),
  (   dict_get(handle_id, Opts, HandleId)
  ->  true
  ;   setting(qh:handle_id, HandleId)
  ),
  HandleId \== '',
  q_query_term(C, Val, QueryTerm),
  http_link_to_id(HandleId, [QueryTerm|Query], Link).



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
  html_call(Content_0).
qh_link(C, Cs, Attrs, Term, Content_0, Opts) -->
  {qh_external_iri(C, Term, Link, Opts)}, !,
  html([
    a([class=Cs,href=Link|Attrs], \html_call(Content_0)),
    \qh_link_external(Term)
  ]).
qh_link(_, _, _, _, Content_0, _) -->
  html_call(Content_0).


qh_link_external(Term) -->
  {is_http_iri(Term)}, !,
  html([" ",\external_link_icon(Term)]).
qh_link_external(_) --> [].
