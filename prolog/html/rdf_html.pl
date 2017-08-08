:- module(
  rdf_html,
  [
    graph_link//1,            % +G
    rdf_html_dataset_term//1, % +D
    rdf_html_dataset_term//2, % +D, +Options
    rdf_html_graph_term//1,   % +G
    rdf_html_graph_term//2,   % +G, +Options
    rdf_html_iri//1,          % +Iri
    rdf_html_iri//2,          % +Iri, +Options
    rdf_html_literal//1,      % +Lit
    rdf_html_literal//2,      % +Lit, +Options
    rdf_html_object//1,       % +O
    rdf_html_object//2,       % +O, +Options
    rdf_html_predicate//1,    % +P
    rdf_html_predicate//2,    % +P, +Options
    rdf_html_subject//1,      % +S
    rdf_html_subject//2,      % +S, +Options
    rdf_html_term//1,         % +Term
    rdf_html_term//2          % +Term, +Options
  ]
).

/** <module> RDF HTML

@author Wouter Beek
@version 2017/05-2017/08
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(date_time)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(html/html_ext)).
:- use_module(library(list_ext)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(uri/uri_ext)).

:- multifile
    html:html_hook//1,
    html:html_hook//2,
    rdf_html_literal_hook//2.

:- rdf_meta
   rdf_html_dataset_term(r, ?, ?),
   rdf_html_dataset_term(r, +, ?, ?),
   rdf_html_graph_term(r, ?, ?),
   rdf_html_graph_term(r, +, ?, ?),
   rdf_html_iri(r, ?, ?),
   rdf_html_iri(r, +, ?, ?),
   rdf_html_object(o, ?, ?),
   rdf_html_object(o, +, ?, ?),
   rdf_html_predicate(r, ?, ?),
   rdf_html_predicate(r, +, ?, ?),
   rdf_html_subject(r, ?, ?),
   rdf_html_subject(r, +, ?, ?),
   rdf_html_term(o, ?, ?),
   rdf_html_term(o, +, ?, ?).





%! graph_link(+G)// is det.

graph_link(G) -->
  {http_link_to_id(navigator_graph_handler, [graph(G)], Uri)},
  html(a(href=Uri, \rdf_html_graph_term(G))).



rdf_html_bnode_iri(Iri) -->
  {uri_comps(Iri, uri(_,_,Segments,_,_))},
  rdf_html_bnode_iri0(Segments).

rdf_html_bnode_iri0([]) --> !, {fail}.
rdf_html_bnode_iri0(['.well-known',genid|Segments]) -->
  {atomic_list_concat(Segments, /, Label)},
  html(["_:",Label]).
rdf_html_bnode_iri0([_|T]) -->
  rdf_html_bnode_iri0(T).



html:html_hook(Options, dataset_term(D)) -->
  rdf_html_dataset_term(D, Options).
html:html_hook(Options, graph_term(G)) -->
  rdf_html_graph_term(G, Options).
html:html_hook(Options, iri(Iri)) -->
  rdf_html_iri(Iri, Options).



%! rdf_html_dataset_term(+D)// is det.
%! rdf_html_dataset_term(+D, +Options)// is det.

rdf_html_dataset_term(D) -->
  rdf_html_dataset_term(D, _{}).


rdf_html_dataset_term(D, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_dataset_term0(D, Options2).

rdf_html_dataset_term0(D, Options) -->
  rdf_html_term0(D, Options).



%! rdf_html_graph_term(+G)// is det.
%! rdf_html_graph_term(+G, +Options)// is det.

rdf_html_graph_term(G) -->
  rdf_html_graph_term(G, _{}).


rdf_html_graph_term(G, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_graph_term0(G, Options2).

rdf_html_graph_term0(G, Options) -->
  rdf_html_term0(G, Options).



%! rdf_html_iri(+Iri)// is det.
%! rdf_html_iri(+Iri, +Options)// is det.

rdf_html_iri(Iri) -->
  rdf_html_iri(Iri, _{}).


rdf_html_iri(Iri, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_iri0(Iri, Options2).

rdf_html_iri0(Iri, Options) -->
  html([
    \rdf_html_iri_internal0(Iri, Options),
    " ",
    \rdf_html_iri_external0(Iri)
  ]).

% Abbreviated notation for IRI.
rdf_html_iri_internal0(Iri, _) -->
  {rdf_equal(rdf:type, Iri)}, !,
  html("a").
rdf_html_iri_internal0(Iri, Options) -->
  {
    Options.iri_abbr == true,
    rdf_global_id(Alias:Local1, Iri), !,
    atom_ellipsis(Local1, Options.max_iri_len, Local2)
  },
  html([Alias,":",Local2]).
% Plain IRI, possibly ellipsed.
rdf_html_iri_internal0(Iri1, Options) -->
  {atom_ellipsis(Iri1, Options.max_iri_len, Iri2)},
  html(Iri2).

rdf_html_iri_external0(Iri) -->
  {is_uri(Iri)}, !,
  html([" ",\external_link(Iri)]).
rdf_html_iri_external0(_) --> [].



%! rdf_html_literal(+Lit)// is det.
%! rdf_html_literal(+Lit, +Options)// is det.

rdf_html_literal(Lit) -->
  rdf_html_literal(Lit, _{}).


rdf_html_literal(Lit, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_literal0(Lit, Options2).

rdf_html_literal0(Lit, Options) -->
  rdf_html_literal_internal0(Lit, Options),
  rdf_html_literal_external0(Lit).

rdf_html_literal_external0(Uri^^D) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(" "),
  {uri_components(Uri, uri_components(Scheme,_,_,_,_))},
  (   {Scheme == mailto}
  ->  mail_icon(Uri)
  ;   {memberchk(Scheme, [http,https])}
  ->  external_link(Uri)
  ).
rdf_html_literal_external0(_) --> [].

% RDF HTML
rdf_html_literal_internal0(V^^D, _) -->
  {rdf_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
rdf_html_literal_internal0(Str@LTag, Options) -->
  {dict_get(show_flag, Options, true)}, !,
  html([
    span(lang(LTag), \html_ellipsis(Str, Options.max_lit_len)),
    " ",
    \flag_icon(LTag)
  ]).
rdf_html_literal_internal0(Str@LTag, Options) --> !,
  html(span(lang=LTag, \html_ellipsis(Str, Options.max_lit_len))).
% XSD boolean
rdf_html_literal_internal0(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD gDay
rdf_html_literal_internal0(Da^^D, _) -->
  {rdf_equal(xsd:gDay, D)}, !,
  html("~d"-[Da]).
% XSD gMonth
rdf_html_literal_internal0(Mo^^D, _) -->
  {rdf_equal(xsd:gMonth, D)}, !,
  html("~d"-[Mo]).
% XSD gYear
rdf_html_literal_internal0(Y^^D, _) -->
  {rdf_equal(xsd:gYear, D)}, !,
  html("~d"-[Y]).
% XSD integer
rdf_html_literal_internal0(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD decimal
rdf_html_literal_internal0(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD double
% XSD float
rdf_html_literal_internal0(V^^D, _) -->
  {(rdf_subdatatype_of(D, xsd:float) ; rdf_subdatatype_of(D, xsd:double))}, !,
  html("~G"-[V]).
% XSD string
rdf_html_literal_internal0(Str^^D, Options) -->
  {rdf_subdatatype_of(D, xsd:string)}, !,
  html_ellipsis(Str, Options.max_lit_len).
% XSD URI
rdf_html_literal_internal0(Uri^^D, _) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(Uri).
% XSD date
% XSD dateTime
% XSD gMonthYear
% XSD gYearMonth
rdf_html_literal_internal0(V^^D1, Options) -->
  {
    % @bug here
    rdf11:xsd_date_time_type(D2),
    rdf_subdatatype_of(D1, D2)
  }, !,
  html_date_time(V, Options).
% Datatype hooks.
rdf_html_literal_internal0(Lit, Options) -->
  rdf_html_literal_hook(Lit, Options), !.
% Other literals for which there is no hook.
% E.g., http://www.opengis.net/ont/geosparql#wktLiteral
rdf_html_literal_internal0(Lit, Options) -->
  {rdf_literal_lexical_form(Lit, Lex)},
  html_ellipsis(Lex, Options.max_lit_len).



%! rdf_html_object(+O)// is det.
%! rdf_html_object(+O, +Options)// is det.

rdf_html_object(O) -->
  rdf_html_object(O, _{}).


rdf_html_object(O, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_object0(O, Options2).


rdf_html_object0(O, Options) -->
  rdf_html_term(O, Options).



%! rdf_html_predicate(+P)// is det.
%! rdf_html_predicate(+P, +Options)// is det.

rdf_html_predicate(P) -->
  rdf_html_predicate(P, _{}).


rdf_html_predicate(P, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_predicate0(P, Options2).


rdf_html_predicate0(P, Options) -->
  rdf_html_iri(P, Options).



%! rdf_html_subject(+S)// is det.
%! rdf_html_subject(+S, +Options)// is det.

rdf_html_subject(S) -->
  rdf_html_subject(S, _{}).


rdf_html_subject(S, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_subject0(S, Options2).


rdf_html_subject0(S, _) -->
  rdf_html_bnode_iri(S), !.
rdf_html_subject0(S, Options) -->
  {rdf_is_iri(S)}, !,
  rdf_html_iri0(S, Options).



%! rdf_html_term(+Term)// is det.
%! rdf_html_term(+Term, +Options)// is det.

rdf_html_term(Term) -->
  rdf_html_term(Term, _{}).


rdf_html_term(Term, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_term0(Term, Options2).

rdf_html_term0(Term, Options) -->
  rdf_html_subject0(Term, Options).
rdf_html_term0(Lit, Options) -->
  {rdf_is_literal(Lit)}, !,
  rdf_html_literal0(Lit, Options).





% HELPERS %

%! rdf_html_options(+Options1, -Options2) is det.

rdf_html_options(Options1, Options2) :-
  DefaultOptions = _{iri_abbr: true, max_iri_len: inf, max_lit_len: inf},
  merge_dicts(DefaultOptions, Options1, Options2).
