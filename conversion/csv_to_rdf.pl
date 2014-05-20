:- module(
  csv_to_rdf,
  [
    csv_to_rdf/4 % +Input:or([atom,stream,url])
                 % +Graph:atom
                 % +NamespacePrefix:atom
                 % +ResourceClassName:atom
  ]
).

/** <module> CSV to RDF

Automatic conversion from CSV to RDF.

@author Wouter Beek
@version 2014/02, 2014/05
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(pure_input)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).

:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(generics(row_ext)).
:- use_module(http(http_download)).
:- use_module(xml(xml_namespace)).
:- use_module(xsd(xsd)). % XML namespace.

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdfs_build)).
:- use_module(plRdf_term(rdf_string)).



%! csv_to_rdf(
%!   +Input:or([atom,stream,url]),
%!   +Graph:atom,
%!   +NamespacePrefix:atom,
%!   +ResourceClassName:atom
%! ) is det.

csv_to_rdf(Stream, Graph, NamespacePrefix, ResourceClassName):-
  is_stream(Stream), !,
  % The XML namespace must be defined.
  xml_current_namespace(NamespacePrefix),

  phrase_from_stream(csv(Rows1), Stream),
  maplist(row_to_list, Rows1, Rows2),
  Rows2 = [Header|Rows3],

  % Convert header row.
gtrace,
  csv_header_to_rdf(Graph, NamespacePrefix, Header, Properties),

  % Convert data rows.
  rdf_global_id(NamespacePrefix:ResourceClassName, ResourceClass),
  maplist(csv_row_to_rdf(Graph, ResourceClass, Properties), Rows3).
csv_to_rdf(Url, Graph, NamespacePrefix, ResourceClassName):-
  uri_components(Url, uri_components(Scheme,_,_,_,_)),
  nonvar(Scheme), !,

  setup_call_cleanup(
    download_to_file([], Url, File),
    csv_to_rdf(File, Graph, NamespacePrefix, ResourceClassName),
    delete_file(File)
  ).
csv_to_rdf(File, Graph, NamespacePrefix, ResourceClassName):-
  % Typecheck: the file exists and we have read access.
  access_file(File, read),
  setup_call_cleanup(
    open(File, read, Stream, []),
    csv_to_rdf(Stream, Graph, NamespacePrefix, ResourceClassName),
    close(Stream)
  ).


% CSV HEADER TO RDF %

csv_header_to_rdf(Graph, NamespacePrefix, Header, Properties):-
  maplist(
    csv_header_entry_to_rdf(Graph, NamespacePrefix),
    Header,
    Properties
  ).

csv_header_entry_to_rdf(Graph, NamespacePrefix, HeaderEntry, Property):-
  dcg_phrase(rdf_property_name, HeaderEntry, PropertyName),
  rdf_global_id(NamespacePrefix:PropertyName, Property),
  
  % @tbd Use rdfs_assert_domain/3.
  %      rdfs_assert_domain(Property, rdfs:'Resource', Graph),
  rdf_assert(Property, rdfs:domain, rdfs:'Resource', Graph),
  
  % @tbd Use rdfs_assert_range/3.
  %      rdfs_assert_range(Property, xsd:string, Graph).
  rdf_assert(Property, rdfs:range, xsd:string, Graph).

rdf_property_name, [45] -->
  ascii_white, !,
  rdf_property_name.
rdf_property_name, ascii_letter_lowercase(_, I) -->
  ascii_letter_uppercase(_, I), !,
  rdf_property_name.
rdf_property_name, [X] -->
  [X], !,
  rdf_property_name.
rdf_property_name --> [].


% CSV ROW TO RDF %

csv_row_to_rdf(Graph, ResourceClass, Properties, Row):-
  % A row is translated into an instance of the given class.
  rdf_bnode(Resource),
  rdf_assert_individual(Resource, ResourceClass, Graph),

  % Assert each cell in the given row.
  maplist(csv_cell_to_rdf(Graph, Resource), Properties, Row).


% CSV CELL TO RDF %

csv_cell_to_rdf(_, _, _, ''):- !.
csv_cell_to_rdf(Graph, Resource, Property, String):-
  % @tbd Use rdf_assert_string/4.
  %      rdf_assert_string(Resource, Property, String, Graph).
  rdf_assert(Resource, Property, literal(type(xsd:string,String)), Graph).

