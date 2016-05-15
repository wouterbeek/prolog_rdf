:- module(
  rdf_csv,
  [
    csv_to_rdf/2 % +Source, +Opts
  ]
).

/** <module> CSV to RDF

Automatic conversion from CSV to RDF.

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_table)).
:- use_module(library(debug)).
:- use_module(library(os/open_any2)).
:- use_module(library(pure_input)).
:- use_module(library(semweb/rdf11)).





%! csv_to_rdf(+Source, +Opts) is det.

csv_to_rdf(Source, Opts) :-
  call_on_stream(Source, {Opts}/[In,M,M]>>stream_csv_to_rdf(In, Opts)).


stream_csv_to_rdf(In, Opts) :-
  phrase_from_stream(csv(Rows, Opts), In),
  gtrace,
  dcg_with_output_to(current_output, dcg_table(Rows)).


/*
  maplist(row_to_list, Rows1, Rows2),
  Rows2 = [Header|Rows3],

  % Convert the header row.
  csv_header_to_rdf(G, TBox, Header, Ps),

  % Convert data rows.
  rdf_global_id(TBox:CName, C),
  maplist(csv_row_to_rdf(ABox, G, C, Ps), Rows3).



%! csv_header_to_rdf(
%!   +Graph:rdf_graph,
%!   +TBox:atom,
%!   +Header:list,
%!   -Properties:list(iri)
%! ) is det.
% Converts the header of an CSV file to a collection of RDF properties.
% These properties denote the relationship between:
%   1. the unnamed entity represented by a data row, and
%   2. the value that appears in a specific cell of that data row.

csv_header_to_rdf(G, TBox, Header, Ps) :-
  maplist(csv_header_entry_to_rdf(G, TBox), Header, Ps).



%! csv_header_entry_to_rdf(
%!   +Graph:rdf_graph,
%!   +TBox:atom,
%!   +HeaderEntry,
%!   -Property:iri
%! ) is det.

csv_header_entry_to_rdf(G, TBox, HeaderEntry, P) :-
  atom_phrase(rdf_property_name, HeaderEntry, LocalName),
  rdf_global_id(TBox:LocalName, P),
  rdfs_assert_domain(P, rdfs:'Resource', G),
  rdfs_assert_range(P, xsd:string, G).



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



%! csv_row_to_rdf(
%!   +ABox:iri,
%!   +Graph:rdf_graph,
%!   +Class:iri,
%!   +Properties:list(iri),
%!   +Row:list
%! ) is det.
% Converts a CSV data row to RDF, using the RDF properties that were created
%  based on the header row.

csv_row_to_rdf(ABox, G, C, Ps, Row) :-
  % A row is translated into an instance of the given class.
  rdf_create_next_resource(ABox, [], C, G, Entry),
  
  % Assert each cell in the given row.
  maplist(csv_cell_to_rdf(G, Entry), Ps, Row).



%! csv_cell_to_rdf(
%!   +Graph:rdf_graph,
%!   +Entry:rdf_bnode,
%!   +Property:iri,
%!   +Value
%! ) is det.
% Converts a CSV cell value to RDF.

% Only graphic values are converted.
csv_cell_to_rdf(G, Entry, P, V) :-
  atom_chars(V, Cs),
  member(C, Cs),
  graphic(C, _, _), !,
  {string_codes(S, Cs)},
  rdf_assert(Entry, P, S, G).
% Non-graphic values are ignored.
csv_cell_to_rdf(_, _, _, Val) :-
  debug(
    csv_to_rdf,
    "Will not convert non-graphic value \"~w\" to RDF.",
    [Val]
  ).
*/
