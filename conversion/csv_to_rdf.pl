:- module(
  csv_to_rdf,
  [
    csv_to_rdf/3 % +File:atom
                 % +Graph:atom
                 % +Namespace:atom
  ]
).

/** <module> CSV to RDF

Automatic conversion from CSV to RDF.

@author Wouter Beek
@tbd
@version 2014/02
*/

:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).



%! csv_to_rdf(+File:atom, +Graph:atom, +Namespace:atom) is det.

csv_to_rdf(File, Graph, Namespace):-
  % Check arguments.
  access_file(File, write),
  \+ rdf_graph(Graph),
  xml_current_namespace(Namespace),
  
  % Read file into datastructure.
  csv_read_file(File, [Header|Rows]),
  
  % Convert header row.
  csv_header_to_rdf(Graph, Namespace, Header),
  
  % Convert data rows.
  maplist(csv_row_to_rdf(Graph, Namespace, Header), Rows).


csv_header_to_rdf(Graph, Namespace, Header):-
  maplist(csv_header_entry_to_rdf(Graph, Namespace), Header).


csv_header_entry_to_rdf(Graph, Namespace, HeaderEntry):-
  rdf_global_id(Namespace:HeaderEntry, Class),
  rdfs_assert_class(Class, Graph).


csv_row_to_rdf(Graph, Namespace, Row):-
  

