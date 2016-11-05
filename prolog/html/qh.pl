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

This assumes that an HTTP handler with id `qh` is defined.

The following option sets the HTTP handler:

  - handle_id(+atom)

  - query(+list(compound))

  - query_key(+atom)

The following options are supported to achieve parity with module
`q_print`:

  - iri_abbr(+boolean)

  - max_iri_len(+or([nonneg,oneof([inf])]))

  - max_lit_len(+or([nonneg,oneof([inf])]))

The following options are specific to this module:

  - show_flag(+boolean)

---

@tbd Achieve parity with option `bnode_map` from module `q_print`.

@author Wouter Beek
@version 2016/02-2016/11
*/

:- use_module(library(atom_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(error)).
:- use_module(library(html/html_date_time)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/js_write)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(q/q_bnode_map)).
:- use_module(library(q/q_dataset_db)).
:- use_module(library(q/q_dataset_api)).
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
:- use_module(library(vocab/dbpedia), []). % DBpedia datatype hook.
:- use_module(library(yall)).

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


qh_alias(Alias, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_alias_outer0(Alias, Opts2).


qh_alias_outer0(Alias, Opts) -->
  qh_link(alias, Alias, qh_alias_inner0(Alias), Opts).


qh_alias_inner0(Alias) -->
  html(Alias).



%! qh_bnode(+BNode)// is det.
%! qh_bnode(+BNode, +Opts)// is det.

qh_bnode(BNode) -->
  qh_bnode(BNode, _{}).


qh_bnode(BNode, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_bnode_outer0(BNode, Opts2).


qh_bnode_outer0(BNode, Opts) -->
  qh_link(bnode, BNode, qh_bnode_inner0(BNode), Opts).


qh_bnode_inner0(BNode) -->
  {q_bnode_map(BNode, Lbl)},
  html(["_:",Lbl]).



%! qh_class(+C)// is det.
%! qh_class(+C, +Opts)// is det.

qh_class(C) -->
  qh_class(C, _{}).


qh_class(C, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_term_outer0(C, Opts2).



%! qh_dataset_term(+D)// is det.
%! qh_dataset_term(+D, +Opts)// is det.

qh_dataset_term(D) -->
  qh_dataset_term(D, _{}).


qh_dataset_term(D, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_dataset_term_outer0(D, Opts2).


qh_dataset_term_outer0(D, Opts) -->
  qh_link(dataset, D, qh_dataset_term_inner0(D, Opts), Opts).


qh_dataset_term_inner0(D, _) -->
  {q_dataset_label(D, Lbl)}, !,
  html(Lbl).
qh_dataset_term_inner0(D, Opts) -->
  qh_iri_inner0(D, Opts).



%! qh_datatype(+D)// is det.
%! qh_datatype(+D, +Opts)// is det.

qh_datatype(D) -->
  qh_datatype(D, _{}).


qh_datatype(D, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_term_outer0(D, Opts2).



%! qh_graph_term(+G)// is det.
%! qh_graph_term(+G, +Opts)// is det.

qh_graph_term(G) -->
  qh_graph_term(G, _{}).


qh_graph_term(G, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_graph_term_outer0(G, Opts2).


qh_graph_term_outer0(G, Opts) -->
  qh_link(graph, G, qh_graph_term_inner0(G, Opts), Opts).


qh_graph_term_inner0(G, Opts) -->
  {
    q_dataset_graph(D, G),
    q_graph_label(G, Str)
  }, !,
  html([\qh_dataset_term_inner0(D, Opts),"/",Str]).
qh_graph_term_inner0(G, Opts) -->
  qh_iri_inner0(G, Opts).



%! qh_instance(+I)// is det.
%! qh_instance(+I, +Opts)// is det.

qh_instance(I) -->
  qh_instance(I, _{}).


qh_instance(I, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_term_outer0(I, Opts2).



%! qh_iri(+Iri)// is det.
%! qh_iri(+Iri, +Opts)// is det.

qh_iri(Iri) -->
  qh_iri(Iri, _{}).


qh_iri(Iri, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_iri_outer0(Iri, Opts2).


qh_iri_outer0(Iri, Opts) -->
  qh_link(iri, Iri, qh_iri_inner0(Iri, Opts), Opts).


% Abbreviated notation for IRI.
qh_iri_inner0(Iri, _) -->
  {rdf_global_id(rdf:type, Iri)}, !,
  html("a").
qh_iri_inner0(Iri, Opts) -->
  {
    Opts.iri_abbr == true,
    rdf_global_id(Alias:Local1, Iri), !,
    atom_ellipsis(Local1, Opts.max_iri_len, Local2)
  },
  html([Alias,":",Local2]).
% Plain IRI, possibly ellipsed.
qh_iri_inner0(Iri1, Opts) -->
  {atom_ellipsis(Iri1, Opts.max_iri_len, Iri2)},
  html(Iri2).



%! qh_literal(+Lit)// is det.
%! qh_literal(+Lit, +Opts)// is det.

qh_literal(Lit) -->
  qh_literal(Lit, _{}).


qh_literal(Lit, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_literal_outer0(Lit, Opts2).


qh_literal_outer0(Lit, Opts) -->
  qh_link(literal, Lit, qh_literal_inner0(Lit, Opts), Opts).


% RDF HTML
qh_literal_inner0(V^^D, _) -->
  {q_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
qh_literal_inner0(Str@LTag, Opts) -->
  {get_dict(show_flag, Opts, true)}, !,
  html([
    span(lang(LTag), \ellipsis(Str, Opts.max_lit_len)),
    " ",
    \flag_icon(LTag)
  ]).
qh_literal_inner0(Str@LTag, Opts) --> !,
  html(span(lang=LTag, \ellipsis(Str, Opts.max_lit_len))).
% XSD boolean
qh_literal_inner0(V^^D, _) -->
  {q_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD gDay
qh_literal_inner0(Da^^D, _) -->
  {rdf_equal(xsd:gDay, D)}, !,
  html("~d"-[Da]).
% XSD gMonth
qh_literal_inner0(Mo^^D, _) -->
  {rdf_equal(xsd:gMonth, D)}, !,
  html("~d"-[Mo]).
% XSD gYear
qh_literal_inner0(Y^^D, _) -->
  {rdf_equal(xsd:gYear, D)}, !,
  html("~d"-[Y]).
% XSD integer
qh_literal_inner0(V^^D, _) -->
  {q_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD decimal
qh_literal_inner0(V^^D, _) -->
  {q_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD double
% XSD float
qh_literal_inner0(V^^D, _) -->
  {(q_subdatatype_of(D, xsd:float) ; q_subdatatype_of(D, xsd:double))}, !,
  html("~G"-[V]).
% XSD string
qh_literal_inner0(Str^^D, Opts) -->
  {q_subdatatype_of(D, xsd:string)}, !,
  ellipsis(Str, Opts.max_lit_len).
% XSD URI
qh_literal_inner0(Uri^^D, _) -->
  {q_subdatatype_of(D, xsd:anyURI)}, !,
  html(Uri).
% XSD date
% XSD dateTime
% XSD gMonthYear
% XSD gYearMonth
qh_literal_inner0(V^^D1, Opts) -->
  {
    q_subdatatype_of(D1, D2),
    rdf11:xsd_date_time_type(D2)
  }, !,
  html_date_time(V, Opts).
% Datatype hooks.
qh_literal_inner0(Lit, Opts) -->
  qh_literal_hook(Lit, Opts), !.
% Other literals for which there is no hook.
% E.g., http://www.opengis.net/ont/geosparql#wktLiteral
qh_literal_inner0(Lit, Opts) -->
  {q_literal_lex(Lit, Lex)},
  html(\ellipsis(Lex, Opts.max_lit_len)).



%! qh_object(+O)// is det.
%! qh_object(+O, +Opts)// is det.

qh_object(O) -->
  qh_object(O, _{}).


qh_object(O, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_term_outer0(O, Opts2).



%! qh_predicate(+P)// is det.
%! qh_predicate(+P, +Opts)// is det.

qh_predicate(P) -->
  qh_predicate(P, _{}).


qh_predicate(P, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_iri_outer0(P, Opts2).



%! qh_property(+Prop)// is det.
%! qh_property(+Prop, +Opts)// is det.

qh_property(Prop) -->
  qh_property(Prop, _{}).


qh_property(Prop, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_term_outer0(Prop, Opts2).



%! qh_property_path(+Props)// is det.
%! qh_property_path(+Props, +Opts)// is det.

qh_property_path(Props) -->
  qh_property_path(Props, _{}).


qh_property_path(Props, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  html_seplist(
    {Opts2}/[Prop]>>qh_term_outer0(Prop, Opts2),
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
    span([
      &(lang),
      \qh_triple(S, P, O, Opts2),
      ", ",
      \qh_graph_term_outer0(G, Opts2),
      &(rang)
    ])
  ).



%! qh_subject(+S)// is det.
%! qh_subject(+S, +Opts)// is det.

qh_subject(S) -->
  qh_subject(S, _{}).


qh_subject(S, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_subject_outer0(S, Opts2).


qh_subject_outer0(BNode, Opts) -->
  {q_is_bnode(BNode)}, !,
  qh_bnode_outer0(BNode, Opts).
qh_subject_outer0(Iri, Opts) -->
  {q_is_iri(Iri)}, !,
  qh_iri_outer0(Iri, Opts).
qh_subject_outer0(S, _) -->
  {type_error(q_subject, S)}.



%! qh_term(+Term)// is det.
%! qh_term(+Term, +Opts)// is det.

qh_term(Term) -->
  qh_term(Term, _{}).


qh_term(Term, Opts1) -->
  {qh_default_options(Opts1, Opts2)},
  qh_term_outer0(Term, Opts2).


qh_term_outer0(Lit, Opts) -->
  {q_is_literal(Lit)}, !,
  qh_literal_outer0(Lit, Opts).
qh_term_outer0(BNode, Opts) -->
  {q_is_bnode(BNode)}, !,
  qh_bnode_outer0(BNode, Opts).
qh_term_outer0(Iri, Opts) -->
  {q_is_iri(Iri)}, !,
  qh_iri_outer0(Iri, Opts).
qh_term_outer0(Term, _) -->
  {type_error(q_term, Term)}.



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
    span([
      &(lang),
      \qh_subject_outer0(S, Opts2),
      ", ",
      \qh_predicate_outer0(P, Opts2),
      ", ",
      \qh_object_outer0(O, Opts2),
      &(rang)
    ])
  ).





% HELPERS %

%! qh_default_options(+Opts1, -Opts2) is det.

qh_default_options(Opts1, Opts2) :-
  setting(qh:handle_id, HandleId),
  DefOpts = _{
    classes: [],
    handle_id: HandleId,
    iri_abbr: true,
    max_iri_len: 30,
    max_lit_len: 75
  },
  merge_dicts(DefOpts, Opts1, Opts2).



%! qh_link(+C, +Term, :Content_0, +Opts)// is det.
%
% Generates a hyperlink in case option `handle_id` is set to something
% other than `none`.  Otherwise, the content is generated without a
% hyperlink.
%
% C is the class that denotes the context in which Term is displayed.

qh_link(_, _, Content_0, Opts) -->
  {Opts.handle_id == none}, !,
  html_call(Content_0).
qh_link(C, Term, Content_0, Opts) -->
  {HandleId = Opts.handle_id}, !,
  {
    dict_get(query_key, Opts, C, Key),
    dict_get(query, Opts, [], Query),
    q_query_term(Key, Term, QTerm),
    http_link_to_id(HandleId, [QTerm|Query], Link)
  },
  html([
    a(href=Link, \html_call(Content_0)),
    \qh_link_external(Term)
  ]).
qh_link(_, _, Content_0, _) -->
  html_call(Content_0).


qh_link_external(Uri^^D) -->
  {q_subdatatype_of(D, xsd:anyURI)}, !,
  html(" "),
  {uri_components(Uri, uri_components(Scheme,_,_,_,_))},
  (   {Scheme == mailto}
  ->  mail_icon(Uri)
  ;   {memberchk(Scheme, [http,https])}
  ->  external_link_icon(Uri)
  ).
qh_link_external(Term) -->
  {is_http_iri(Term)}, !,
  html([" ",\external_link_icon(Term)]).
qh_link_external(_) --> [].
