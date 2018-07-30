:- module(
  rdf_html,
  [
    rdf_html_iri//1,          % +Iri
    rdf_html_iri//2,          % +Iri, +Options
    rdf_html_literal//1,      % +Literal
    rdf_html_literal//2,      % +Literal, +Options
    rdf_html_nonliteral//1,   % +NonLiteral
    rdf_html_nonliteral//2,   % +NonLiteral, +Options
    rdf_html_term//1,         % +Term
    rdf_html_term//2,         % +Term, +Options
    rdf_html_triple//2,       % +Uri, +Triple
    rdf_html_triple//3,       % +Uri, +Triple, +Options
    rdf_html_triple_table//2, % +Uri, +Triples
    rdf_html_triple_table//3, % +Uri, ?G, +Triples
    rdf_html_triple_table//4  % +Uri, ?G, +Triples, +Options
  ]
).
:- reexport(library(html/html_ext)).

/** <module> RDF HTML

# Options

| *Option*    | *Type*   |
|-------------|----------|
| format      | ntriples |
| iri_abbr    | Boolean  |
| max_iri_len | [0,∞)    |
| max_lit_len | [0,∞)    |

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(semweb/rdf11), []).
:- use_module(library(sgml)).
:- use_module(library(yall)).

:- use_module(library(atom_ext)).
:- use_module(library(date_time)).
:- use_module(library(dcg)).
:- use_module(library(dict)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/http_server)).
:- use_module(library(http/rdf_http)).
:- use_module(library(list_ext)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(uri_ext)).
:- use_module(library(xsd/xsd)).

:- multifile
    html:html_hook//1,
    html:html_hook//2,
    html_doc:custom_param_type//1,
    rdf_html_literal_hook//3.

html_doc:custom_param_type(Spec) -->
  {memberchk(rdf_term, Spec)}, !,
  html("RDF term").

:- rdf_meta
   rdf_html_iri(r, ?, ?),
   rdf_html_iri(r, +, ?, ?),
   rdf_html_literal(o, ?, ?),
   rdf_html_literal(o, +, ?, ?),
   rdf_html_literal_external_(r, ?, +, ?, ?),
   rdf_html_literal_internal_(r, ?, +, +, ?, ?),
   rdf_html_nonliteral(r, ?, ?),
   rdf_html_nonliteral(r, +, ?, ?),
   rdf_html_term(o, ?, ?),
   rdf_html_term(o, +, ?, ?),
   rdf_html_triple(+, t, ?, ?),
   rdf_html_triple(+, t, +, ?, ?),
   rdf_html_triple_table(+, r, t, ?, ?),
   rdf_html_triple_table(+, r, t, +, ?, ?).





%! rdf_html_graph_link(+G:rdf_graph)// is det.

rdf_html_graph_link(G) -->
  {
    rdf_http_query([g(G)], Query),
    http_link_to_id(navigator_graph_handler, Query, Uri)
  },
  html(a(href=Uri, \rdf_html_iri(G))).



%! rdf_html_bnode(+BNode:atom)// is det.

rdf_html_bnode(BNode) -->
  html(BNode).



%! rdf_html_iri(+Iri)// is det.
%! rdf_html_iri(+Iri, +Options)// is det.

rdf_html_iri(Iri) -->
  rdf_html_iri(Iri, _{}).


rdf_html_iri(Iri, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_iri_(Iri, Options2).

rdf_html_iri_(Iri, Options) -->
  html([
    \rdf_html_iri_internal_(Iri, Options),
    " ",
    \rdf_html_iri_external_(Iri)
  ]).

rdf_html_iri_internal_(Iri, Options) -->
  {
    dict_get(iri_abbr, Options, true, true),
    rdf_prefix_iri(Alias:Local1, Iri), !,
    dict_get(max_iri_len, Options, ∞, Length),
    (   Length == ∞
    ->  Local2 = Local1
    ;   atom_ellipsis(Local1, Length, Local2)
    )
  },
  html([Alias,":",Local2]).
% Plain IRI, possibly ellipsed.
rdf_html_iri_internal_(Iri1, Options) -->
  {
    dict_get(max_iri_len, Options, ∞, Length),
    (   Length == ∞
    ->  Iri2 = Iri1
    ;   atom_ellipsis(Iri1, Options.max_iri_len, Iri2)
    )
  },
  html(["<",Iri2,">"]).

rdf_html_iri_external_(Iri) -->
  {is_uri(Iri)}, !,
  html([" ",\external_link(Iri)]).
rdf_html_iri_external_(_) --> [].



%! rdf_html_literal(+Literal)// is det.
%! rdf_html_literal(+Literal, +Options)// is det.

rdf_html_literal(Literal) -->
  rdf_html_literal(Literal, _{}).


rdf_html_literal(Literal, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_literal_(Literal, Options2).

rdf_html_literal_(Literal, Options) -->
  {dict_get(format, Options, ntuples)}, !,
  rdf_html_literal_ntuples(Literal).
rdf_html_literal_(Literal, Options) -->
  {rdf_literal(D, LTag, Lex, Literal)},
  rdf_html_literal_internal_(D, LTag, Lex, Options),
  rdf_html_literal_external_(D, LTag, Lex).

rdf_html_literal_external_(xsd:anyURI, _, Uri) --> !,
  html(" "),
  {uri_components(Uri, uri_components(Scheme,_,_,_,_))},
  (   {Scheme == mailto}
  ->  mail_icon(Uri)
  ;   {memberchk(Scheme, [http,https])}
  ->  external_link(Uri)
  ).
rdf_html_literal_external_(_, _, _) --> [].

% hook
rdf_html_literal_internal_(D, _, Lex, Options) -->
  rdf_html_literal_hook(D, Lex, Options), !.
% rdf:HTML
rdf_html_literal_internal_(rdf:'HTML', _, Lex, _) --> !,
  html(\[Lex]).
% rdf:langString
rdf_html_literal_internal_(rdf:langString, LTag, Lex, Options) --> !,
  {dict_get(show_flag, Options, true)}, !,
  html([
    span(lang(LTag), \html_lexical_(Lex, Options)),
    " ",
    \flag_icon(LTag)
  ]).
rdf_html_literal_internal_(rdf:langString, LTag, Lex, Options) --> !,
  html(span(lang=LTag, \html_lexical_(Lex, Options))).
% xsd:boolean
rdf_html_literal_internal_(xsd:boolean, _, Lex, _) -->
  html(Lex).
% xsd:decimal: before other numeric types
rdf_html_literal_internal_(xsd:decimal, _, Lex, _) --> !,
  {xsd_lexical_value(xsd:decimal, Lex, N)},
  html("~w"-[N]).
% xsd:byte
% xsd:double
% xsd:float
% xsd:int
% xsd:integer
% xsd:long,
% xsd:negativeInteger
% xsd:nonNegativeInteger
% xsd:nonPositiveInteger
% xsd:positiveInteger
% xsd:short
% xsd:unsignedByte
% xsd:unsignedInt,
% xsd:unsignedLong
% xsd:unsignedShort
rdf_html_literal_internal_(D, _, Lex, _) -->
  {
    rdf11:xsd_numerical(D, _, Type), !,
    xsd_number_string(N, Lex),
    (Type == integer -> Format = "~D" ; Format = "~w")
  },
  html(Format-[N]).
% xsd:date
% xsd:dateTime
% xsd:gDay
% xsd:gMonth
% xsd:gMonthDay
% xsd:gYear
% xsd:gYearMonth
% xsd:time
rdf_html_literal_internal_(D, _, Lex, Options) -->
  {
    rdf11:xsd_date_time_type(D), !,
    xsd_time_string(DateTime, D, Lex),
    xsd_date_time_to_dt(DateTime, D, DT)
  },
  html_date_time(DT, Options).
% xsd:string
rdf_html_literal_internal_(xsd:string, _, Lex, Options) --> !,
  html_lexical_(Lex, Options).
% xsd:anyURI
rdf_html_literal_internal_(xsd:anyUri, _, Uri, _) --> !,
  html(Uri).
% other
rdf_html_literal_internal_(_, _, Lex, Options) -->
  html_lexical_(Lex, Options).

html_lexical_(Lex, Options) -->
  {dict_get(max_lit_len, Options, ∞, Length)},
  (   {Length == ∞}
  ->  html(Lex)
  ;   html_ellipsis(Lex, Length)
  ).

rdf_html_literal_ntuples(literal(lang(LTag,Lex))) -->
  html(["\"",Lex,"\"@",LTag]).
rdf_html_literal_ntuples(literal(type(D,Lex))) -->
  html(["\"",Lex,"\"^^<",D,">"]).



%! rdf_html_nonliteral(+S)// is det.
%! rdf_html_nonliteral(+S, +Options)// is det.

rdf_html_nonliteral(S) -->
  rdf_html_nonliteral(S, _{}).


rdf_html_nonliteral(S, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_nonliteral_(S, Options2).


rdf_html_nonliteral_(S, _) -->
  {rdf_is_bnode(S)}, !,
  rdf_html_bnode(S).
rdf_html_nonliteral_(S, _) -->
  {rdf_is_bnode_iri(S)}, !,
  rdf_html_well_known_iri(S).
rdf_html_nonliteral_(S, Options) -->
  {rdf_is_iri(S)}, !,
  rdf_html_iri_(S, Options).



%! rdf_html_predicate_(+P:iri, +Options:list(compound))// is det.

% Abbreviated notation for IRI.
rdf_html_predicate_(Iri, _) -->
  {rdf_equal(rdf:type, Iri)}, !,
  html("a").
rdf_html_predicate_(Iri, Options) -->
  rdf_html_iri(Iri, Options).



%! rdf_html_term(+Term)// is det.
%! rdf_html_term(+Term, +Options)// is det.

rdf_html_term(Term) -->
  rdf_html_term(Term, _{}).


rdf_html_term(Term, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_term_(Term, Options2).

rdf_html_term_(Term, Options) -->
  rdf_html_nonliteral_(Term, Options).
rdf_html_term_(Literal, Options) -->
  rdf_html_literal_(Literal, Options).



%! rdf_html_triple(+Uri:atom, +Triple:compound)// is det.
%! rdf_html_triple(+Uri:atom, +Triple:compound,
%!                 +Options:list(compound))// is det.

rdf_html_triple(Uri, Triple) -->
  rdf_html_triple(Uri, Triple, _{}).


rdf_html_triple(Uri, rdf(S,P,O), Options1) -->
  {
    rdf_html_options(Options1, Options2),
    rdf_http_query([s(S)], SQuery),
    uri_comp_set(query, Uri, SQuery, SUri),
    rdf_http_query([p(P)], PQuery),
    uri_comp_set(query, Uri, PQuery, PUri),
    rdf_http_query([o(O)], OQuery),
    uri_comp_set(query, Uri, OQuery, OUri)
  },
  html(
    tr([
      td(a(href=SUri, \rdf_html_nonliteral_(S, Options2))),
      td(a(href=PUri, \rdf_html_predicate_(P, Options2))),
      td(a(href=OUri, \rdf_html_term_(O, Options2)))
    ])
  ).
    


%! rdf_html_triple_table(+Uri:atom, +Triples:list(compound))// is det.
%! rdf_html_triple_table(+Uri:atom, ?G:rdf_graph, +Triples:list(compound))// is det.
%! rdf_html_triple_table(+Uri:atom, ?G:rdf_graph, +Triples:list(compound),
%!                       +Options:list(compound))// is det.

rdf_html_triple_table(Uri, Triples) -->
  rdf_html_triple_table(Uri, _, Triples).


rdf_html_triple_table(Uri, G, Triples) -->
  rdf_html_triple_table(Uri, G, Triples, _{}).


rdf_html_triple_table(Uri, G, Triples, Options1) -->
  {rdf_html_options(Options1, Options2)},
  html_table(
    \html_table_header_row(["Subject","Predicate","Object"]),
    \html_maplist(
      [Triple]>>rdf_html_triple_table_row(Uri, G, Triple, Options2),
      Triples
    )
  ).

rdf_html_triple_table_row(Uri, G, rdf(S,P,O), Options) -->
  {
    rdf_http_query([s(S),g(G)], SQuery),
    uri_comp_set(query, Uri, SQuery, SUri),
    rdf_http_query([p(P),g(G)], PQuery),
    uri_comp_set(query, Uri, PQuery, PUri),
    rdf_http_query([o(O),g(G)], OQuery),
    uri_comp_set(query, Uri, OQuery, OUri)
  },
  html(
    tr([
      td(a(href=SUri, \rdf_html_nonliteral_(S, Options))),
      td(a(href=PUri, \rdf_html_predicate_(P, Options))),
      td(a(href=OUri, \rdf_html_term_(O, Options)))
    ])
  ).



%! rdf_html_well_known_iri(+Iri:atom)// is det.

rdf_html_well_known_iri(Iri) -->
  {
    uri_comps(Iri, uri(_,_,['.well-known',genid|Segments],_,_)),
    atomics_to_string(Segments, "/", Local)
  },
  html(["well-known:",Local]).





% HELPERS %

%! rdf_html_options(+Options1, -Options2) is det.

rdf_html_options(Options1, Options2) :-
  DefaultOptions = _{iri_abbr: true, max_iri_len: ∞, max_lit_len: ∞},
  merge_dicts(DefaultOptions, Options1, Options2).
