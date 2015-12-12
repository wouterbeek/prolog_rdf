:- module(
  rdf_html_table,
  [
    rdf_html_quadruple_table//5, % ?Subject:or([bnode,iri])
                                 % ?Predicate:iri
                                 % ?Object:rdf_term
                                 % ?Graph:atom
                                 % +Options:list(compound)
    rdf_html_table//2, % +Rows:list
                       % +Options:list(compound)
    rdf_html_triple_table//5 % ?Subject:or([bnode,iri])
                             % ?Predicate:iri
                             % ?Object:rdf_term
                             % ?Graph:atom
                             % +Options:list(compound)
  ]
).

/** <module> RDF HTML table

Generates HTML tables with RDF content.

@author Wouter Beek
@version 2015/08, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(html/rdf_html_term)).
:- use_module(library(html/content/html_pl)).
:- use_module(library(html/element/html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_stats)).
:- use_module(library(typecheck)).

:- rdf_register_prefix(rdf_table, 'http://rdf_table.org/').

:- predicate_options(rdf_html_quadruple_table//5, 5, [
     max_rows(+nonneg),
     pass_to(rdf_html_statement_table//5, 5)
   ]).
:- predicate_options(rdf_html_table//2, 2, [
     caption(+callable),
     graph(+atom),
     header_spec(+list(char)),
     location(+atom),
     pass_to(html_direct_table//2, 2),
     pass_to(rdf_html_term//2, 2)
   ]).
:- predicate_options(rdf_html_triple_table//5, 5, [
     max_rows(+nonneg),
     pass_to(rdf_html_statement_table//5, 5)
   ]).

:- rdf_meta(rdf_html_quadruple_table(r,r,o,?,+,?,?)).
:- rdf_meta(rdf_html_table(+,+,?,?)).
:- rdf_meta(rdf_html_triple_table(r,r,o,?,+,?,?)).





%! rdf_html_quadruple_table(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! )// is det.
% The following options are supported:
%   * max_rows(+nonneg)

rdf_html_quadruple_table(S, P, O, G, Opts1) -->
  {
    option(max_rows(Max), Opts1, 100),
    findnsols(Max, [S,P,O,graph(G)], rdf(S, P, O, G), Rows0),
    sort(Rows0, Rows),
    merge_options([header_spec([s,p,o,g])], Opts1, Opts2)
  },
  rdf_html_statement_table(S, P, O, G, Rows, Opts2).



%! rdf_html_table(+Rows:list, +Options:list(compound))// is det.
% The following options are supported:
%   * `graph(+RdfGraph:atom)`
%     The RDF graph that is used for retrieving resources via hyperlinks.
%   * `header_spec(+list(char))`
%     Support for often occurring header rows.
%     A boolean is passed on to wl_table//2.
%     Often occurring header rows are atoms that consist of
%     the here enumerated characters,
%     where the characters correspond to columns,
%     in the order in which they occur.
%   * Other options are passed on to wl_table//2.
%
% The following characters are supported for
% atomic values of the header row option:
%   | `g` | Graph     |
%   | `l` | Literal   |
%   | `o` | Object    |
%   | `p` | Predicate |
%   | `s` | Subject   |
% A boolean value can be used as well, and is processed by wl_table//2.

% Do not fail for tables with no data rows whatsoever.
rdf_html_table(Rows0, Opts1) -->
  {
    process_header_row(Rows0, Rows, Opts1),
    merge_options([cell(rdf_html_term0(Opts1))], Opts1, Opts2)
  },
  html_direct_table(Rows, Opts2).
rdf_html_term0(Opts, T) -->
  rdf_html_term(T, Opts).



%! rdf_html_triple_table(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! )// is det.
% The following options are supported:
%   * max_rows(+nonneg)

rdf_html_triple_table(S, P, O, G, Opts1) -->
  {
    option(max_rows(Max), Opts1, 100),
    findnsols(Max, [S,P,O], rdf(S, P, O, G), Rows0),
    sort(Rows0, Rows),
    merge_options([header_spec([s,p,o])], Opts1, Opts2)
  },
  rdf_html_statement_table(S, P, O, G, Rows, Opts2).





% HELPERS %

process_header_row(T, [head(H)|T], Opts):-
  T \= [head(_)|_],
  option(header_spec(Spec), Opts), !,
  maplist(header_spec, Spec, H).
process_header_row(Rows, Rows, _).

header_spec('C', 'Class'    ).
header_spec( d , 'Datatype' ).
header_spec( g , 'Graph'    ).
header_spec( l , 'Literal'  ).
header_spec( o , 'Object'   ).
header_spec( p , 'Predicate').
header_spec('P', 'Property' ).
header_spec( s , 'Subject'  ).
header_spec( t , 'Term'     ).



rdf_html_statement_table(S, P, O, G, Rows, Opts1) -->
  {
    merge_options(
      Opts1,
      [
        caption(rdf_html_statement_table_caption(S, P, O, G, Opts1)),
        graph(G)
      ],
      Opts2
    )
  },
  rdf_html_table(Rows, Opts2).

% Exactly one term.
rdf_html_statement_table_caption(S, P, O, G, Opts) -->
  {
    include(nonvar, [S,P,O], [Term]),
    (   Term == S
    ->  Pos = subject
    ;   Term == P
    ->  Pos = predicate
    ;   Term == O
    ->  Pos = object
    )
  },
  html([
    'Triples in which ',
    \rdf_html_term_in_graph(Term, G, Opts),
    ' occurs in the ',Pos,' position.'
  ]).
% No term, only a graph.
rdf_html_statement_table_caption(_, _, _, G, Opts) -->
  {nonvar(G)}, !,
  {
    option(max_rows(Max), Opts, 100),
    rdf_number_of_triples(G, N)
  },
  html([
    'Triples in graph ',
    \rdf_html_graph(G, Opts),
    ' (showing ',
    \html_pl_term(thousands_integer(Max)),
    ' out of ',
    \html_pl_term(thousands_integer(N)),
    'results).'
  ]).
% Neither term nor graph.
rdf_html_statement_table_caption(_, _, _, _, _) -->
  html('Triples').
