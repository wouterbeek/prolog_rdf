:- module(
  rdf_table,
  [
    rdf_assert_table/6 % +Graph:atom
                       % +Caption:atom
                       % +ColumnHeaders:list(atom)
                       % +RowHeaders:list(atom)
                       % +Rows:list(compound)
                       % -Table:iri
  ]
).

/** <module> RDF table

A simple RDF vocabulary for representing tables.

@author Wouter Beek
@version 2014/02-2014/03
*/

:- use_module(generics(row_ext)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_list)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(rdf_term(rdf_string)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf_table, 'http://www.wouterbeek.com/rdf_table#').



%! rdf_assert_table(
%!   +Graph:atom,
%!   +Caption:atom,
%!   +ColumnHeaders:list(atom),
%!   +RowHeaders:list(atom),
%!   +Rows:list(compound),
%!   -Table:iri
%! ) is det.
% Asserts a table in a simple RDF vocabulary.

rdf_assert_table(Graph, Caption, ColumnHeaders, RowHeaders, Rows, Table):-
  % Assert caption.
  rdf_assert_string(Table, rdf_table:caption, Caption, Graph),
  
  % Assert headers.
  rdf_assert_column_headers(Graph, Table, ColumnHeaders, ColumnList),
  rdf_assert_row_headers(Graph, Table, RowHeaders, RowList),
  
  % Assert rows.
  forall(
    nth0(Y, Rows, Row),
    rdf_assert_row(Graph, Table, ColumnList, Y-RowList, Row)
  ).


%! rdf_assert_row(
%!   +RdfGraph:atom,
%!   +RdfTable:iri,
%!   +ColumnList:iri,
%!   +RowPair:pair(nonneg,iri),
%!   +Row:compound
%! ) is det.
% Asserts a table row in a simple RDF vocabulary.

rdf_assert_row(Graph, Table, ColumnList, Y-RowList, Row):-
  forall(
    nth0_column(X, Row, Value),
    rdf_assert_cell(Graph, Table, X-ColumnList, Y-RowList, Value)
  ).


%! rdf_assert_cell(
%!   +Graph:atom,
%!   +Table:iri,
%!   +ColumnPair:pair(nonneg,iri),
%!   +RowPair:pair(nonneg,iri),
%!   +Value
%! ) is det.
% Asserts a table cell in a simple RDF vocabulary.

rdf_assert_cell(G, Table, X-ColumnList, Y-RowList, Value):-
  % A table cell is a blank node.
  rdf_bnode(Cell),
  
  % Assert the column header.
  rdf_list_nth0(X, ColumnList, ColumnHeader),
  rdf_assert_string(Cell, rdf_table:column, ColumnHeader, G),
  
  % Assert the row header.
  rdf_list_nth0(Y, RowList, RowHeader),
  rdf_assert_string(Cell, rdf_table:row, RowHeader, G),
  
  % Assert the cell value.
  rdf_assert_datatype(Cell, rdf:value, Value, xsd:float, G),
  
  % Relate cell to table.
  rdf_assert(Table, rdf_table:cell, Cell, G).


%! rdf_assert_column_headers(
%!   +Graph:atom,
%!   +Table:iri,
%!   +ColumnHeaders:list(atom),
%!   -ColumnHeadersList:iri
%! ) is det.
% Asserts the column headers of a table.

rdf_assert_column_headers(Graph, Table, ColumnHeaders, ColumnList):-
  rdf_global_id(rdf_table:columns, Predicate),
  rdf_assert_headers(Graph, Table, Predicate, ColumnHeaders, ColumnList).


%! rdf_assert_row_headers(
%!   +Graph:atom,
%!   +Table:iri,
%!   +RowHeaders:list(atom),
%!   -RowHeadersList:iri
%! ) is det.
% Asserts the row headers of a table.

rdf_assert_row_headers(Graph, Table, RowHeaders, RowList):-
  rdf_global_id(rdf_table:rows, Predicate),
  rdf_assert_headers(Graph, Table, Predicate, RowHeaders, RowList).


%! rdf_assert_headers(
%!   +Graph:atom,
%!   +Table:iri,
%!   +Predicate:iri,
%!   +RowHeaders:list(atom),
%!   -HeaderList:iri
%! ) is det.
% Asserts either column or row headers of a table, depending on `Predicate`.

rdf_assert_headers(Graph, Table, Predicate, Headers, HeaderList):-
  rdf_assert_list([datatype(xsd:string)], Headers, HeaderList, Graph),
  rdf_assert(Table, Predicate, HeaderList, Graph).

