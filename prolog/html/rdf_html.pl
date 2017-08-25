:- module(
  rdf_html,
  [
  % GRAPHS
    rdf_html_graph//2,        % +Uri, +Triples
  % STATEMENTS
    rdf_html_triple//2,       % +Uri, +Triple
  % TERMS
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
    rdf_html_term//2,         % +Term, +Options
    rdf_html_triple_table//3  % +Uri, ?G, +Triples
  ]
).
:- reexport(library(html/html_ext)).

/** <module> RDF HTML

@author Wouter Beek
@version 2017/05-2017/08
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(date_time)).
:- use_module(library(dict_ext)).
:- use_module(library(html/html_date_time_machine)).
:- use_module(library(http/http_server)).
:- use_module(library(list_ext)).
:- use_module(library(semweb/rdf_date_time)).
:- use_module(library(semweb/rdf_ext)).
:- use_module(library(uri/uri_ext)).

:- multifile
    html:html_hook//1,
    html:html_hook//2,
    rdf_html_literal_hook//2.

:- rdf_meta
 % GRAPHS
   rdf_html_graph(+, t, ?, ?),
 % STATEMENTS
   rdf_html_triple(+, t, ?, ?),
 % TERMS
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
   rdf_html_term(o, +, ?, ?),
   rdf_html_triple_table(+, r, t).





% GRAPHS

%! rdf_html_graph(+Uri:atom, +Triples:list(compound))// is det.

rdf_html_graph(Uri, Triples) -->
  table(
    \table_header_row(["Subject","Predicate","Object"]),
    \html_maplist(rdf_html_triple(Uri), Triples)
  ).





% STATEMENTS

%! rdf_html_triple(+Uri:atom, +Triple:compound)// is det.

rdf_html_triple(Uri, rdf(S,P,O)) -->
  {
    maplist(rdf_term_to_atom, [S,P,O], [AtomS,AtomP,AtomO]),
    maplist(
      uri_comp_set(query, Uri),
      [[subject(AtomS)],[predicate(AtomP)],[object(AtomO)]],
      [UriS,UriP,UriO]
    )
  },
  html(
    tr([
      td(a(href=UriS, \rdf_html_subject(S))),
      td(a(href=UriP, \rdf_html_predicate(P))),
      td(a(href=UriO, \rdf_html_object(O)))
    ])
  ).
    




% TERMS

%! graph_link(+G)// is det.

graph_link(G) -->
  {http_link_to_id(navigator_graph_handler, [graph(G)], Uri)},
  html(a(href=Uri, \rdf_html_graph_term(G))).



rdf_html_bnode_iri(Iri) -->
  {uri_comps(Iri, uri(_,_,Segments,_,_))},
  rdf_html_bnode_iri_(Segments).

rdf_html_bnode_iri_([]) --> !, {fail}.
rdf_html_bnode_iri_(['.well-known',genid|Segments]) -->
  {atomic_list_concat(Segments, /, Label)},
  html(["_:",Label]).
rdf_html_bnode_iri_([_|T]) -->
  rdf_html_bnode_iri_(T).



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
  rdf_html_dataset_term_(D, Options2).

rdf_html_dataset_term_(D, Options) -->
  rdf_html_term_(D, Options).



%! rdf_html_graph_term(+G)// is det.
%! rdf_html_graph_term(+G, +Options)// is det.

rdf_html_graph_term(G) -->
  rdf_html_graph_term(G, _{}).


rdf_html_graph_term(G, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_graph_term_(G, Options2).

rdf_html_graph_term_(G, Options) -->
  rdf_html_term_(G, Options).



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

% Abbreviated notation for IRI.
rdf_html_iri_internal_(Iri, _) -->
  {rdf_equal(rdf:type, Iri)}, !,
  html("a").
rdf_html_iri_internal_(Iri, Options) -->
  {
    Options.iri_abbr == true,
    rdf_global_id(Alias:Local1, Iri), !,
    atom_ellipsis(Local1, Options.max_iri_len, Local2)
  },
  html([Alias,":",Local2]).
% Plain IRI, possibly ellipsed.
rdf_html_iri_internal_(Iri1, Options) -->
  {atom_ellipsis(Iri1, Options.max_iri_len, Iri2)},
  html(Iri2).

rdf_html_iri_external_(Iri) -->
  {is_uri(Iri)}, !,
  html([" ",\external_link(Iri)]).
rdf_html_iri_external_(_) --> [].



%! rdf_html_literal(+Lit)// is det.
%! rdf_html_literal(+Lit, +Options)// is det.

rdf_html_literal(Lit) -->
  rdf_html_literal(Lit, _{}).


rdf_html_literal(Lit, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_literal_(Lit, Options2).

rdf_html_literal_(Lit, Options) -->
  rdf_html_literal_internal_(Lit, Options),
  rdf_html_literal_external_(Lit).

rdf_html_literal_external_(Uri^^D) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(" "),
  {uri_components(Uri, uri_components(Scheme,_,_,_,_))},
  (   {Scheme == mailto}
  ->  mail_icon(Uri)
  ;   {memberchk(Scheme, [http,https])}
  ->  external_link(Uri)
  ).
rdf_html_literal_external_(_) --> [].

% RDF HTML
rdf_html_literal_internal_(V^^D, _) -->
  {rdf_subdatatype_of(D, rdf:'HTML')}, !,
  html(\[V]).
% RDF language-tagged string
rdf_html_literal_internal_(Str@LTag, Options) -->
  {dict_get(show_flag, Options, true)}, !,
  html([
    span(lang(LTag), \html_ellipsis(Str, Options.max_lit_len)),
    " ",
    \flag_icon(LTag)
  ]).
rdf_html_literal_internal_(Str@LTag, Options) --> !,
  html(span(lang=LTag, \html_ellipsis(Str, Options.max_lit_len))).
% XSD boolean
rdf_html_literal_internal_(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:boolean)}, !,
  html("~a"-[V]).
% XSD gDay
rdf_html_literal_internal_(Da^^D, _) -->
  {rdf_equal(xsd:gDay, D)}, !,
  html("~d"-[Da]).
% XSD gMonth
rdf_html_literal_internal_(Mo^^D, _) -->
  {rdf_equal(xsd:gMonth, D)}, !,
  html("~d"-[Mo]).
% XSD gYear
rdf_html_literal_internal_(Y^^D, _) -->
  {rdf_equal(xsd:gYear, D)}, !,
  html("~d"-[Y]).
% XSD integer
rdf_html_literal_internal_(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:integer)}, !,
  html("~D"-[V]).
% XSD decimal
rdf_html_literal_internal_(V^^D, _) -->
  {rdf_subdatatype_of(D, xsd:decimal)}, !,
  html("~w"-[V]).
% XSD double
% XSD float
rdf_html_literal_internal_(V^^D, _) -->
  {(rdf_subdatatype_of(D, xsd:float) ; rdf_subdatatype_of(D, xsd:double))}, !,
  html("~G"-[V]).
% XSD string
rdf_html_literal_internal_(Str^^D, Options) -->
  {rdf_subdatatype_of(D, xsd:string)}, !,
  html_ellipsis(Str, Options.max_lit_len).
% XSD URI
rdf_html_literal_internal_(Uri^^D, _) -->
  {rdf_subdatatype_of(D, xsd:anyURI)}, !,
  html(Uri).
% XSD date
% XSD dateTime
% XSD gMonthYear
% XSD gYearMonth
rdf_html_literal_internal_(Value1^^D1, Options) -->
  {
    % @bug here
    rdf11:xsd_date_time_type(D2),
    rdf_subdatatype_of(D1, D2), !,
    rdf_date_time_to_dt(Value1, Value2)
  },
  html_date_time(Value2, Options).
% Datatype hooks.
rdf_html_literal_internal_(Lit, Options) -->
  rdf_html_literal_hook(Lit, Options), !.
% Other literals for which there is no hook.
% E.g., http://www.opengis.net/ont/geosparql#wktLiteral
rdf_html_literal_internal_(Lit, Options) -->
  {rdf_literal_lexical_form(Lit, Lex)},
  html_ellipsis(Lex, Options.max_lit_len).



%! rdf_html_object(+O)// is det.
%! rdf_html_object(+O, +Options)// is det.

rdf_html_object(O) -->
  rdf_html_object(O, _{}).


rdf_html_object(O, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_object_(O, Options2).


rdf_html_object_(O, Options) -->
  rdf_html_term(O, Options).



%! rdf_html_predicate(+P)// is det.
%! rdf_html_predicate(+P, +Options)// is det.

rdf_html_predicate(P) -->
  rdf_html_predicate(P, _{}).


rdf_html_predicate(P, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_predicate_(P, Options2).


rdf_html_predicate_(P, Options) -->
  rdf_html_iri(P, Options).



%! rdf_html_subject(+S)// is det.
%! rdf_html_subject(+S, +Options)// is det.

rdf_html_subject(S) -->
  rdf_html_subject(S, _{}).


rdf_html_subject(S, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_subject_(S, Options2).


rdf_html_subject_(S, _) -->
  rdf_html_bnode_iri(S), !.
rdf_html_subject_(S, Options) -->
  {rdf_is_iri(S)}, !,
  rdf_html_iri_(S, Options).



%! rdf_html_term(+Term)// is det.
%! rdf_html_term(+Term, +Options)// is det.

rdf_html_term(Term) -->
  rdf_html_term(Term, _{}).


rdf_html_term(Term, Options1) -->
  {rdf_html_options(Options1, Options2)},
  rdf_html_term_(Term, Options2).

rdf_html_term_(Term, Options) -->
  rdf_html_subject_(Term, Options).
rdf_html_term_(Lit, Options) -->
  {rdf_is_literal(Lit)}, !,
  rdf_html_literal_(Lit, Options).



%! rdf_html_triple_table(+Uri:atom, ?G:atom, +Triples:list(compound))// is det.

rdf_html_triple_table(Uri, G, Triples) -->
  table(
    \table_header_row(["Subject","Predicate","Object"]),
    \html_maplist(rdf_html_triple_table_row(Uri, G), Triples)
  ).

rdf_html_triple_table_row(Uri, G, rdf(S,P,O)) -->
  {
    maplist(rdf_term_to_atom, [S,P,O], [AtomS,AtomP,AtomO]),
    (var(G) -> Query = [] ; Query = [graph(G)]),
    maplist(
      uri_comp_set(query, Uri),
      [[subject(AtomS)|Query],[predicate(AtomP)|Query],[object(AtomO)|Query]],
      [UriS,UriP,UriO]
    )
  },
  html(
    tr([
      td(a(href=UriS, \rdf_html_subject(S))),
      td(a(href=UriP, \rdf_html_predicate(P))),
      td(a(href=UriO, \rdf_html_object(O)))
    ])
  ).





% HELPERS %

%! rdf_html_options(+Options1, -Options2) is det.

rdf_html_options(Options1, Options2) :-
  DefaultOptions = _{iri_abbr: true, max_iri_len: inf, max_lit_len: inf},
  merge_dicts(DefaultOptions, Options1, Options2).
