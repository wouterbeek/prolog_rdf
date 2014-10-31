:- module(
  void_db,
  [
    void_dataset_location/3, % +VoidGraph:atom
                             % +VoidDataset:iri
                             % -DatadumpFile:atom
    void_dataset/2 % +VoidGraph:atom
                   % -VoidDataset:iri
  ]
).

/** <module> VoID DB

Generic support for VoID, used by other VoID modules.

@author Wouter Beek
@version 2013/11, 2014/03
*/

:- use_module(library(lists), except([delete/3])).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(typecheck)).
:- use_module(os(file_ext)).

:- use_module(plHttp(http_download)).

:- use_module(plRdf_ser(rdf_load_any)).
:- use_module(plRdf_term(rdf_term)).

:- rdf_register_prefix(void, 'http://rdfs.org/ns/void#').



%! void_init(?RdfGraph:atom) is det.
% Loads the VoID vocabulary.

void_init(Graph):-
  void_url(Url),
  rdf_load_any(Url, [graph(Graph)]).



%! void_graph_datasets(+VoidGraph:atom, -VoidDataset:iri) is semidet.
% Translates between VoID graphs and the RDF datasets described by them.

void_dataset(VoidGraph, VoidDataset):-
  rdfs_individual_of(VoidDataset, void:'Dataset'),
  once(rdf_term(VoidDataset, VoidGraph)).


void_dataset_location(VoidGraph, VoidDataset, DatadumpFile):-
  % Every dataset has exactly one datadump property.
  % @tbd Is this assumption correct?l
  rdf(VoidDataset, void:dataDump, DatadumpLocation, VoidGraph),
  (
    is_of_type(iri, DatadumpLocation)
  ->
    % Store locally.
    download_to_file(DatadumpLocation, DatadumpFile)
  ;
    is_absolute_file_name(DatadumpLocation)
  ->
    DatadumpFile = DatadumpLocation
  ;
    rdf_graph_property(VoidGraph, source(VoidFile)),
    file_components(VoidFile, VoidDirectory, _, _),
    relative_file_path(DatadumpFile, VoidDirectory, DatadumpLocation)
  ).


void_url('http://vocab.deri.ie/void.ttl').

