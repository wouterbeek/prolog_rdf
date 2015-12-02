:- module(
  rdf_html_table,
  [
    rdf_html_quadruple_table//5, % ?Subject:or([bnode,iri])
                                 % ?Predicate:iri
                                 % ?Object:rdf_term
                                 % ?Graph:atom
                                 % +Options:list(compound)
    rdf_html_table//2, % +Table:iri
                       % +Options:list(compound)
    rdf_html_table//3, % :Caption
                       % +Rows:list(list(ground))
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
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(html/element/html_table)).
:- use_module(library(http/html_write)).
:- use_module(library(option)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(typecheck)).

:- rdf_register_prefix(rdf_table, 'http://rdf_table.org/').

:- predicate_options(rdf_html_quadruple_table//5, 5, [
     max_rows(+nonneg),
     pass_to(rdf_html_statement_table//6, 6)
   ]).
:- predicate_options(rdf_html_table//2, 2, [
     location(+atom),
     pass_to(rdf_html_table//3, 3)
   ]).
:- predicate_options(rdf_html_table//3, 3, [
     graph(+atom),
     header_row(+or([atom,boolean])),
     location(+atom),
     pass_to(html_direct_table//2, 2),
     pass_to(rdf_html_term0//2, 1)
   ]).
:- predicate_options(rdf_html_term0//2, 1, [
     pass_to(rdf_html_term//2, 2)
   ]).
:- predicate_options(rdf_html_triple_table//5, 5, [
     max_rows(+nonneg),
     pass_to(rdf_html_statement_table//6, 6)
   ]).

:- rdf_meta(rdf_html_quadruple_table(r,r,o,?,+,?,?)).
:- rdf_meta(rdf_html_table(r,+,?,?)).
:- rdf_meta(rdf_html_table(+,t,+,?,?)).
:- rdf_meta(rdf_html_triple_table(r,r,o,?,+,?,?)).

:- html_meta(rdf_html_table(html,+,+,?,?)).





%! rdf_html_quadruple_table(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   +Options:list(compound)
%! )// is det.
% The following options are supported:
%   * max_rows(+nonneg)

rdf_html_quadruple_table(S, P, O, G, Opts) -->
  {
    option(max_rows(Max), Opts, 100),
    findnsols(Max, [S,P,O,graph(G)], user:rdf(S, P, O, G), Rows0),
    sort(Rows0, Rows)
  },
  rdf_html_statement_table(S, P, O, G, spog, Rows, Opts).



%! rdf_html_table(+Table:or([bnode,iri]), +Options:list(compound))// is det.
% RDF table resources are supported by [rdf_table].
%
% Options are defined by rdf_html_table//3.

rdf_html_table(Table, Opts) -->
  {
    option(header_column(HasHeaderColumn), Opts),
    option(header_row(HasHeaderRow), Opts),
    rdf_literal(Table, rdf_table:caption, xsd:string, Caption),
    rdf(Table, rdf_table:columns, Columns),
    rdf_list_raw(Columns, ColumnHeaders),
    rdf_has(Table, rdf_table:rows, Rows),
    rdf_list_raw(Rows, RowHeaders),
    rdf_table_get_rows(
      Table,
      HasHeaderColumn,
      ColumnHeaders,
      RowHeaders,
      Rows1
    ),
    (   HasHeaderRow == true
    ->  Rows2 = [ColumnHeaders|Rows1]
    ;   Rows2 = Rows1
    )
  },
  rdf_html_table(rdf_html_term(Caption, Opts), Rows2, Opts).



%! rdf_html_table(
%!   :Caption,
%!   +Rows:list(list(ground)),
%!   +Options:list(compound)
%! )// is det.
% The following options are supported:
%   * `graph(+RdfGraph:atom)`
%     The RDF graph that is used for retrieving resources via hyperlinks.
%   * `header_row(+or([atom,boolean]))`
%     Support for often occurring header rows.
%     A boolean is passed on to wl_table//2.
%     Often occurring header rows are atoms that consist of
%     the here enumerated characters,
%     where the characters correspond to columns,
%     in the order in which they occur.
%   * `location(Location:iri)`
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
rdf_html_table(Caption, Rows0, Opts1) -->
  {
    % Process the header row option.
    select_option(header_row(HeaderSpec), Opts1, Opts2, false),
    (   boolean(HeaderSpec)
    ->  HeaderRowPresent = HeaderSpec,
        Rows = Rows0
    ;   atom_phrase('*'(header_row_preset, HeaderRow, []), HeaderSpec),
        HeaderRowPresent = true,
        Rows = [HeaderRow|Rows0]
    ),
    merge_options([header_row(HeaderRowPresent)], Opts2, Opts3),
    
    % Retrieve the location id.
    % Relative URIs are resolved relative to this location.
    option(location(LocationId), Opts3, _VAR),
    merge_options(
      [caption(Caption),cell(rdf_html_term0(LocationId, Opts3))],
      Opts3,
      Opts4
    )
  },
  html_direct_table(Rows, Opts4).
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

rdf_html_triple_table(S, P, O, G, Opts) -->
  {
    option(max_rows(Max), Opts, 100),
    findnsols(Max, [S,P,O], user:rdf(S, P, O, G), Rows0),
    sort(Rows0, Rows)
  },
  rdf_html_statement_table(S, P, O, G, spo, Rows, Opts).





% HELPERS %

%! header_row_preset(-HeaderName:atom)// is det.
% Grammar rules for singular header names.

header_row_preset('Graph')     --> "g".
header_row_preset('Literal')   --> "l".
header_row_preset('Object')    --> "o".
header_row_preset('Predicate') --> "p".
header_row_preset('Subject')   --> "s".



rdf_html_statement_table(S, P, O, G, HeaderSpec, Rows, Opts1) -->
  merge_options([graph(G),header_row(HeaderSpec)], Opts1, Opts2),
  rdf_html_table(
    rdf_html_statement_table_caption(S, P, O, G, Opts1),
    Rows,
    Opts2
  ).



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
    'RDF triples in which ',
    \rdf_html_term_in_graph(Term, G, Opts),
    ' occurs in the ',Pos,' position.'
  ]).
rdf_html_statement_table_caption(_, _, _, _, _) -->
  html('RDF triples').



%! rdf_table_get_rows(
%!   +Table:or([bnode,iri]),
%!   +HasHeaderColumn:boolean,
%!   +ColumnHeaders:list(iri),
%!   +RowHeaders:list(iri),
%!   -Rows:list(list(ground))
%! ) is det.

rdf_table_get_rows(_, _, _, [], []):- !.
rdf_table_get_rows(
  Table,
  HasHeaderColumn,
  ColumnHeaders,
  [RowHeader|RowHeaders],
  [Row2|Rows]
):-
  rdf_table_get_row(Table, ColumnHeaders, RowHeader, Row1),
  (   HasHeaderColumn == true
  ->  Row2 = [RowHeader|Row1]
  ;   Row2 = Row1
  ),
  rdf_table_get_rows(Table, HasHeaderColumn, ColumnHeaders, RowHeaders, Rows).



%! rdf_table_get_row(
%!   +Table:or([bnode,iri]),
%!   +ColumnHeaders:list(iri),
%!   +RowHeader:iri,
%!   -Row:list(ground)
%! ) is det.

rdf_table_get_row(_, [], _, []):- !.
rdf_table_get_row(Table, [ColumnHeader|ColumnHeaders], RowHeader, [H|T]):-
  rdf_has(Table, rdf_table:cell, Cell),
  rdf_literal(Cell, rdf_table:column, xsd:strin, ColumnHeader),
  rdf_literal(Cell, rdf_table:row, xsd:string, RowHeader),
  rdf_has(Cell, rdf:value, H), !,
  rdf_table_get_row(Table, ColumnHeaders, RowHeader, T).
