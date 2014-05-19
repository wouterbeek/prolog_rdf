:- module(
  csv_to_rdf,
  [
    csv_to_rdf/4 % +Input:or([atom,stream])
                 % +Graph:atom
                 % +Namespace:atom
                 % +ResourceClass:iri
  ]
).

/** <module> CSV to RDF

Automatic conversion from CSV to RDF.

@author Wouter Beek
@tbd
@version 2014/02, 2014/05
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(pure_input)).
:- use_module(library(semweb/rdf_db)).

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)). % XML namespace.

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdfs_build)).
:- use_module(plRdf_term(rdf_string)).



%! csv_to_rdf(
%!   +Input:or([atom,stream]),
%!   +Graph:atom,
%!   +Namespace:atom,
%!   +ResourceClass:iri
%! ) is det.

csv_to_rdf(Stream, Graph, Namespace, ResourceClass):-
  is_stream(Stream), !,
  % Typecheck: the RDF graph does not exist; the XML namespace does.
  \+ rdf_graph(Graph),
  xml_current_namespace(Namespace),

  phrase_from_stream(csv(Rows1), Stream),
  Rows1 = [Header|Rows2],

  % Convert header row.
  csv_header_to_rdf(Graph, Namespace, Header, Properties),

  % Convert data rows.
  maplist(csv_row_to_rdf(Graph, ResourceClass, Properties), Rows2).
csv_to_rdf(File, Graph, Namespace, ResourceClass):-
  % Typecheck: the file exists and we have read access.
  access_file(File, read),
  setup_call_cleanup(
    open(File, read, Stream, []),
    csv_to_rdf(Stream, Graph, Namespace, ResourceClass),
    close(Stream)
  ).


csv_header_to_rdf(Graph, Namespace, Header, Properties):-
  maplist(csv_header_entry_to_rdf(Graph, Namespace), Header, Properties).


csv_header_entry_to_rdf(Graph, Namespace, HeaderEntry, Property):-
  dcg_phrase(rdf_property_name, HeaderEntry, PropertyName),
  rdf_global_id(Namespace:PropertyName, Property),
  rdfs_assert_domain(Property, rdfs:'Resource', Graph),
  rdfs_assert_range(Property, xsd:string, Graph).

rdf_property_name, `_` -->
  ascii_white, !.
rdf_property_name, ascii_letter_lowercase(_, I) -->
  ascii_letter_uppercase(_, I), !.
rdf_property_name, [X] --> [X], !.
rdf_property_name --> [].


csv_row_to_rdf(Graph, ResourceClass, Properties, Row):-
  % A row is translated into an instance of the given class.
  rdf_bnode(Resource),
  rdf_assert_individual(Resource, ResourceClass, Graph),

  % Assert each cell in the given row.
  maplist(csv_cell_to_rdf(Graph, Resource), Properties, Row).


csv_cell_to_rdf(_, _, _, ''):- !.
csv_cell_to_rdf(Graph, Resource, Property, String):-
  rdf_assert_string(Resource, Property, String, Graph).

