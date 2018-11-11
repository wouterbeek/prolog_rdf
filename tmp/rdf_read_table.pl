:- module(
  rdf_read_table,
  [
    rdf_read_table/2 % +Table:rdf_term, +Opts
  ]
).

/** <module> RDF read table

@author Wouter Beek
@version 2015/12-2016/01
*/

:- use_module(library(option)).

:- rdf_meta
   rdf_read_table(o, +, ?, ?).





%! rdf_read_table(+Table:rdf_term, +Opts)// is det.
% RDF table resources are supported by [rdf_table].
%
% Options are defined by rdf_table//3.

rdf_read_table(Table, Opts) -->
  {
    option(header_column(HasHeaderColumn), Opts),
    option(header_row(HasHeaderRow), Opts),
    rdf_has(Table, rdf_table:caption, Caption),
    rdf_has(Table, rdf_table:columns, Columns),
    rdf_list(Columns, ColumnHeaders),
    rdf_has(Table, rdf_table:rows, Rows),
    rdf_list(Rows, RowHeaders),
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
  rdf_table(rdf_dcg_term(Caption), Rows2, Opts).


%! rdf_table_get_rows(
%!   +Table:rdf_term,
%!   +HasHeaderColumn:boolean,
%!   +ColumnHeaders:list(iri),
%!   +RowHeaders:list(iri),
%!   -Rows:list(list(ground))
%! ) is det.

rdf_table_get_rows(_, _, _, [], []) :- !.
rdf_table_get_rows(
  Table,
  HasHeaderColumn,
  ColumnHeaders,
  [RowHeader|RowHeaders],
  [Row2|Rows]
) :-
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

rdf_table_get_row(_, [], _, []) :- !.
rdf_table_get_row(Table, [ColumnHeader|ColumnHeaders], RowHeader, [H|T]) :-
  rdf_has(Table, rdf_table:cell, Cell),
  rdf_has(Cell, rdf_table:column, ColumnHeader),
  rdf_has(Cell, rdf_table:row, RowHeader),
  rdf_has(Cell, rdf:value, H), !,
  rdf_table_get_row(Table, ColumnHeaders, RowHeader, T).
