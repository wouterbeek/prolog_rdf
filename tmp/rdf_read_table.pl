:- module(
  rdf_read_table,
  [
    rdf_html_read_table/2 % +Table:rdf_term
                          % +Options:list(compound)
  ]
).

/** <module> RDF read table

@author Wouter Beek
@version 2015/12
*/

:- rdf_meta(rdf_html_read_table(o,+,?,?)).





%! rdf_html_read_table(+Table:rdf_term, +Options:list(compound))// is det.
% RDF table resources are supported by [rdf_table].
%
% Options are defined by rdf_html_table//3.

rdf_html_read_table(Table, Opts) -->
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
    ->  Rows2 = [head(ColumnHeaders)|Rows1]
    ;   Rows2 = Rows1
    )
  },
  rdf_html_table(rdf_html_term(Caption, Opts), Rows2, Opts).


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
