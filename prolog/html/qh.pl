:- module(
  qh,
  [
    qh_alias//1,         % +Alias
    qh_bnode//1,         % +BNode
    qh_class//1,         % +C
    qh_class//2,         % +C,             +Opts
    qh_dataset_term//1,  % +D
    qh_dataset_term//2,  % +D,             +Opts
    qh_graph_term//1,    % +G
    qh_graph_term//2,    % +G,             +Opts
    qh_instance//1,      % +I
    qh_instance//2,      % +I,             +Opts
    qh_iri//1,           % +Iri
    qh_iri//2,           % +Iri,           +Opts
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
    qh_triple//4         % +S, +P, +O,     +Opts
  ]
).

/** <module> Quine HTML basics

Generates end user-oriented HTML representations of RDF data.

The following options are supported to achieve parity with module
`q_print`:

  - iri_abbr(+boolean)

  - max_iri_len(+or([nonneg,oneof([inf])]))

  - max_lit_len(+or([nonneg,oneof([inf])]))

The following options are specific to this module:

  - show_flag(+boolean)

---

@author Wouter Beek
@version 2016/02-2016/11
*/

:- use_module(library(atom_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(lists)).
:- use_module(library(q/q_dataset_db)).
:- use_module(library(q/q_dataset_api)).
:- use_module(library(q/q_datatype)).
:- use_module(library(q/q_graph)).
:- use_module(library(q/q_term)).
:- use_module(library(semweb/rdf11)). % rdf_meta/1
:- use_module(library(typecheck)).
:- use_module(library(vocab/dbpedia), []). % DBpedia datatype hook.
:- use_module(library(yall)).

:- multifile
    html:html_hook//1,
    html:html_hook//2,
    qh_literal_hook//2,
    rdf11:in_ground_type_hook/3,
    rdf11:out_type_hook/3.

:- rdf_meta
   qh_class(r, ?, ?),
   qh_class(r, +, ?, ?),
   qh_dataset_term(r, ?, ?),
   qh_dataset_term(r, +, ?, ?),
   qh_graph_term(r, ?, ?),
   qh_graph_term(r, +, ?, ?),
   qh_instance(r, ?, ?),
   qh_instance(r, +, ?, ?),
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

html:html_hook(Opts, q_dataset_term(D)) -->
  qh_dataset_term(D, Opts).
html:html_hook(Opts, q_graph_term(G)) -->
  qh_graph_term(G, Opts).
html:html_hook(Opts, q_iri(Iri)) -->
  qh_iri(Iri, Opts).





%! qh_alias(+Alias)// is det.

qh_alias(Alias) -->
  html(Alias).



%! qh_bnode(+BNode)// is det.

qh_bnode(BNode) -->
  {rdf_global_id(bnode:Local, BNode)},
  html(["_:",Local]).



%! qh_class(+C)// is det.
%! qh_class(+C, +Opts)// is det.

qh_class(C) -->
  qh_class(C, _{}).


qh_class(C, Opts) -->
  qh_term(C, Opts).



%! qh_dataset_term(+D)// is det.
%! qh_dataset_term(+D, +Opts)// is det.

qh_dataset_term(D) -->
  qh_dataset_term(D, _{}).


qh_dataset_term(D, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_dataset_term0(D, Opts2).

qh_dataset_term0(D, _) -->
  {q_dataset_label(D, Lbl)}, !,
  html(Lbl).
qh_dataset_term0(D, Opts) -->
  qh_term0(D, Opts).



%! qh_graph_term(+G)// is det.
%! qh_graph_term(+G, +Opts)// is det.

qh_graph_term(G) -->
  qh_graph_term(G, _{}).


qh_graph_term(G, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_graph_term0(G, Opts2).

qh_graph_term0(G, Opts) -->
  {
    q_dataset_graph(D, G),
    q_graph_label(G, Str)
  }, !,
  html([\qh_dataset_term0(D, Opts),"/",Str]).
qh_graph_term0(G, Opts) -->
  qh_term0(G, Opts).



%! qh_instance(+I)// is det.
%! qh_instance(+I, +Opts)// is det.

qh_instance(I) -->
  qh_instance(I, _{}).


qh_instance(I, Opts) -->
  qh_term(I, Opts).



%! qh_iri(+Iri)// is det.
%! qh_iri(+Iri, +Opts)// is det.

qh_iri(Iri) -->
  qh_iri(Iri, _{}).


qh_iri(Iri, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_iri0(Iri, Opts2),
  qh_iri_external0(Iri).

% Abbreviated notation for IRI.
qh_iri0(Iri, _) -->
  {rdf_global_id(rdf:type, Iri)}, !,
  html("a").
qh_iri0(Iri, Opts) -->
  {
    Opts.iri_abbr == true,
    rdf_global_id(Alias:Local1, Iri), !,
    atom_ellipsis(Local1, Opts.max_iri_len, Local2)
  },
  html([Alias,":",Local2]).
% Plain IRI, possibly ellipsed.
qh_iri0(Iri1, Opts) -->
  {atom_ellipsis(Iri1, Opts.max_iri_len, Iri2)},
  html(Iri2).

qh_iri_external0(Iri) -->
  {is_http_iri(Iri)}, !,
  html([" ",\external_link_icon(Iri)]).
qh_iri_external0(_) --> [].



%! qh_literal(+Lit)// is det.
%! qh_literal(+Lit, +Opts)// is det.

qh_literal(Lit) -->
  qh_literal(Lit, _{}).


qh_literal(Lit, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_literal0(Lit, Opts2).

qh_literal0(Lit, Opts) -->
  qh_literal00(Lit, Opts),
  qh_literal_external0(Lit).

% RDF HTML
qh_literal00(V^^D, _) -->
  {q_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
qh_literal00(Str@LTag, Opts) -->
  {get_dict(show_flag, Opts, true)}, !,
  html([
    span(lang(LTag), \ellipsis(Str, Opts.max_lit_len)),
    " ",
    \flag_icon(LTag)
  ]).
qh_literal00(Str@LTag, Opts) --> !,
  html(span(lang=LTag, \ellipsis(Str, Opts.max_lit_len))).
% XSD boolean
qh_literal00(V^^D, _) -->
  {q_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD gDay
qh_literal00(Da^^D, _) -->
  {rdf_equal(xsd:gDay, D)}, !,
  html("~d"-[Da]).
% XSD gMonth
qh_literal00(Mo^^D, _) -->
  {rdf_equal(xsd:gMonth, D)}, !,
  html("~d"-[Mo]).
% XSD gYear
qh_literal00(Y^^D, _) -->
  {rdf_equal(xsd:gYear, D)}, !,
  html("~d"-[Y]).
% XSD integer
qh_literal00(V^^D, _) -->
  {q_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD decimal
qh_literal00(V^^D, _) -->
  {q_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD double
% XSD float
qh_literal00(V^^D, _) -->
  {(q_subdatatype_of(D, xsd:float) ; q_subdatatype_of(D, xsd:double))}, !,
  html("~G"-[V]).
% XSD string
qh_literal00(Str^^D, Opts) -->
  {q_subdatatype_of(D, xsd:string)}, !,
  ellipsis(Str, Opts.max_lit_len).
% XSD URI
qh_literal00(Uri^^D, _) -->
  {q_subdatatype_of(D, xsd:anyURI)}, !,
  html(Uri).
% XSD date
% XSD dateTime
% XSD gMonthYear
% XSD gYearMonth
qh_literal00(V^^D1, Opts) -->
  {
    q_subdatatype_of(D1, D2),
    rdf11:xsd_date_time_type(D2)
  }, !,
  html_date_time(V, Opts).
% Datatype hooks.
qh_literal00(Lit, Opts) -->
  qh_literal_hook(Lit, Opts), !.
% Other literals for which there is no hook.
% E.g., http://www.opengis.net/ont/geosparql#wktLiteral
qh_literal00(Lit, Opts) -->
  {q_literal_lex(Lit, Lex)},
  html(\ellipsis(Lex, Opts.max_lit_len)).

qh_literal_external0(Uri^^D) -->
  {q_subdatatype_of(D, xsd:anyURI)}, !,
  html(" "),
  {uri_components(Uri, uri_components(Scheme,_,_,_,_))},
  (   {Scheme == mailto}
  ->  mail_icon(Uri)
  ;   {memberchk(Scheme, [http,https])}
  ->  external_link_icon(Uri)
  ).
qh_literal_external0(_) --> [].



%! qh_object(+O)// is det.
%! qh_object(+O, +Opts)// is det.

qh_object(O) -->
  qh_object(O, _{}).


qh_object(O, Opts) -->
  qh_term(O, Opts).



%! qh_predicate(+P)// is det.
%! qh_predicate(+P, +Opts)// is det.

qh_predicate(P) -->
  qh_predicate(P, _{}).


qh_predicate(P, Opts) -->
  qh_iri(P, Opts).



%! qh_property(+Prop)// is det.
%! qh_property(+Prop, +Opts)// is det.

qh_property(Prop) -->
  qh_property(Prop, _{}).


qh_property(Prop, Opts) -->
  qh_term(Prop, Opts).



%! qh_property_path(+Props)// is det.
%! qh_property_path(+Props, +Opts)// is det.

qh_property_path(Props) -->
  qh_property_path(Props, _{}).


qh_property_path(Props, Opts) -->
  html_seplist({Opts}/[Prop]>>qh_property(Prop, Opts), " ", Props).



%! qh_quad(+S, +P, +O, +G)// is det.
%! qh_quad(+S, +P, +O, +G, +Opts)// is det.

qh_quad(S, P, O, G) -->
  qh_quad(S, P, O, G, _{}).


qh_quad(S, P, O, G, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_quad0(S, P, O, G, Opts2).

qh_quad0(S, P, O, G, Opts) -->
  html(
    span([
      &(lang),
      \qh_triple0(S, P, O, Opts),
      ", ",
      \qh_graph_term0(G, Opts),
      &(rang)
    ])
  ).



%! qh_subject(+S)// is det.
%! qh_subject(+S, +Opts)// is det.

qh_subject(S) -->
  qh_subject(S, _{}).


qh_subject(S, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_subject0(S, Opts2).

qh_subject0(BNode, _) -->
  {q_is_bnode(BNode)}, !,
  qh_bnode(BNode).
qh_subject0(Iri, Opts) -->
  {q_is_iri(Iri)}, !,
  qh_iri0(Iri, Opts).



%! qh_term(+Term)// is det.
%! qh_term(+Term, +Opts)// is det.

qh_term(Term) -->
  qh_term(Term, _{}).


qh_term(Term, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_term0(Term, Opts2).

qh_term0(Term, Opts) -->
  qh_subject0(Term, Opts).
qh_term0(Lit, Opts) -->
  {q_is_literal(Lit)}, !,
  qh_literal0(Lit, Opts).



%! qh_triple(+Triple)// is det.
%! qh_triple(+Triple, +Opts)// is det.
%! qh_triple(+S, +P, +O)// is det.
%! qh_triple(+S, +P, +O, +Opts)// is det.

qh_triple(Triple) -->
  qh_triple(Triple, _{}).


qh_triple(rdf(S,P,O), Opts) -->
  qh_triple(S, P, O, Opts).


qh_triple(S, P, O) -->
  qh_triple(S, P, O, _{}).


qh_triple(S, P, O, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_triple0(S, P, O, Opts2).

qh_triple0(S, P, O, Opts) -->
  html(
    span([
      &(lang),
      \qh_subject0(S, Opts),
      ", ",
      \qh_predicate0(P, Opts),
      ", ",
      \qh_object0(O, Opts),
      &(rang)
    ])
  ).





% HELPERS %

%! qh_default_options(+Opts1, -Opts2) is det.

qh_default_options(Opts1, Opts2) :-
  DefOpts = _{iri_abbr: true, max_iri_len: 30, max_lit_len: 75},
  merge_dicts(DefOpts, Opts1, Opts2).
