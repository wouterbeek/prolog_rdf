:- module(
  csv_to_rdf,
  [
    csv_to_rdf/4 % +Input:stream
                 % +Graph:atom
                 % +Prefix:atom
                 % +ClassName:atom
  ]
).

/** <module> CSV to RDF

Automatic conversion from CSV to RDF.

@author Wouter Beek
@version 2014/02, 2014/05, 2014/08, 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(pure_input)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_unicode)).
:- use_module(plc(generics/row_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).





%! csv_to_rdf(
%!   +Input:stream,
%!   +Graph:atom,
%!   +Prefix:atom,
%!   +ClassName:atom
%! ) is det.
% @throws type_error if the given prefix is not a registered RDF prefix.

csv_to_rdf(_, _, Prefix, _):-
  \+ rdf_current_prefix(Prefix, _), !,
  type_error(rdf_prefix, Prefix).
csv_to_rdf(In, Graph, Prefix, ClassName):-
  is_stream(In), !,

  % Parse the CSV data.
  phrase_from_stream(csv(Rows1), In),
  maplist(row_to_list, Rows1, Rows2),
  Rows2 = [Header|Rows3],

  % Convert the header row.
  csv_header_to_rdf(Graph, Prefix, Header, Properties),

  % Convert data rows.
  rdf_global_id(Prefix:ClassName, Class),
  maplist(csv_row_to_rdf(Graph, Class, Properties), Rows3).



% CSV HEADER TO RDF %

%! csv_header_to_rdf(
%!   +Graph:atom,
%!   +Prefix:atom,
%!   +Header:list,
%!   -Properties
%! ) is det.
% Converts the header of an CSV file to a collection of RDF properties.
% These properties denote the relationship between:
%   1. the unnamed entity represented by a data row, and
%   2. the value that appears in a specific cell of that data row.

csv_header_to_rdf(Graph, Prefix, Header, Properties):-
  maplist(
    csv_header_entry_to_rdf(Graph, Prefix),
    Header,
    Properties
  ).



%! csv_header_entry_to_rdf(
%!   +Graph:atom,
%!   +Prefix:atom,
%!   +HeaderEntry,
%!   -Property:iri
%! ) is det.

csv_header_entry_to_rdf(Graph, Prefix, HeaderEntry, Property):-
  dcg_phrase(rdf_property_name, HeaderEntry, LocalName),
  rdf_global_id(Prefix:LocalName, Property),
  rdfs_assert_domain(Property, rdfs:'Resource', Graph),
  rdfs_assert_range(Property, xsd:string, Graph).



%! rdf_property_name// .
% Converts the header name into the local name of the RDF term denoting
%  the RDF property.
%
% The following conversions are applied:
%   - Unicode line terminators and whites are replaced by underscores.
%   - Unicode uppercase letters are converted to lowercase letters.

rdf_property_name, underscore -->
  (   line_terminator
  ;   punctuation
  ;   white
  ), !,
  rdf_property_name.
rdf_property_name, [Lower] -->
  letter(Code), !,
  {code_type(Code, to_upper(Lower))},
  rdf_property_name.
rdf_property_name, [X] -->
  [X], !,
  rdf_property_name.
rdf_property_name --> [].



% CSV ROW TO RDF %

%! csv_row_to_rdf(
%!   +Graph:atom,
%!   +Class:iri,
%!   +Properties:list(iri),
%!   +Row:list(atom)
%! ) is det.
% Converts a CSV data row to RDF, using the RDF properties that were created
%  based on the header row.

csv_row_to_rdf(Graph, Class, Properties, Row):-
  % A row is translated into an instance of the given class.
  rdf_bnode(Entry),
  rdf_assert_instance(Entry, Class, Graph),

  % Assert each cell in the given row.
  maplist(csv_cell_to_rdf(Graph, Entry), Properties, Row).



% CSV CELL TO RDF %

%! csv_cell_to_rdf(
%!   +Graph:atom,
%!   +Entry:bnode,
%!   +Property:iri,
%!   +Value:atom
%! ) is det.
% Converts a CSV cell value to RDF.

% Only graphic values are converted.
csv_cell_to_rdf(Graph, Entry, Property, Value):-
  atom_chars(Value, Codes),
  member(Code, Codes),
  graphic(Code, _, _), !,
  rdf_assert_simple_literal(Entry, Property, Value, Graph).
% Non-graphic values are ignored.
csv_cell_to_rdf(_, _, _, Value):-
  debug(
    csv_to_rdf,
    'Will not convert non-graphic value "~w" to RDF.',
    [Value]
  ).
