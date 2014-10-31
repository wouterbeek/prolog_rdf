:- module(
  void_stat,
  [
    void_update/1, % +VoidGraph:atom
    void_update_dataset/2 % +VoidGraph:atom
                          % +RdfGraph:atom
  ]
).

/** <module> VoID statistics

Asserts statistics for VoID descriptions.

@author Wouter Beek
@version 2013/03-2013/05, 2013/09-2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(thread_ext)).
:- use_module(void(void_db)). % RDF prefix registration.

:- use_module(plRdf(rdf_graph_name)).
:- use_module(plRdf(rdf_stat)).
:- use_module(plRdf_term(rdf_dateTime)).
:- use_module(plRdf_term(rdf_datatype)).
:- use_module(plRdf_term(rdf_string)).

:- use_module(plXsd_datetime(xsd_dateTime_ext)).

:- rdf_register_prefix(dc, 'http://purl.org/dc/elements/1.1/').



%! void_update(+VoidGraph:atom) is det.

void_update(VoidGraph):-
  % NO THREADS
  forall(
    void_dataset(VoidGraph, VoidDataset),
    void_update_dataset(VoidGraph, VoidDataset)
  ).
/*
  % THREADS
  forall_thread(
    (
      void_dataset(VoidGraph, VoidDataset),
      format(atom(Msg), 'Saving VoID dataset ~w.', [VoidDataset])
    ),
    void_update_dataset(VoidGraph, VoidDataset),
    void_file,
    Msg
  ).
*/


%! void_update_dataset(+VoidGraph:atom, +VoidDataset:iri) is det.

%void_update_dataset(_, VoidDataset):-
%  rdf_graph_property(VoidDataset, modified(false)), !.
void_update_dataset(VoidGraph, VoidDataset):-
  % dc:modified.
  rdf_update_today(VoidDataset, dc:modified, VoidGraph),

  % void:classes
  count_classes(VoidDataset, NumberOfClasses),
  rdf_overwrite_datatype(VoidDataset, void:classes, NumberOfClasses,
      xsd:integer, VoidGraph),

  % void:distinctObjects
  count_objects(_, _, VoidDataset, NumberOfObjects),
  rdf_overwrite_datatype(VoidDataset, void:distinctObjects, NumberOfObjects,
      xsd:integer, VoidGraph),

  % void:distinctSubjects
  count_subjects(_, _, VoidDataset, NumberOfSubjects),
  rdf_overwrite_datatype(VoidDataset, void:distinctSubjects, NumberOfSubjects,
       xsd:integer, VoidGraph),

/*
  % void:entities
  (
    rdf_string(VoidDataset, void:uriRegexPattern, RegularExpression, VoidGraph)
  ->
    aggregate_all(
      set(Entity),
      (
        rdf(Entity, _, _, VoidDataset),
        Entity=~RegularExpression
      ),
      Entities
    ),
    length(Entities, NumberOfEntities),
    rdf_overwrite_datatype(VoidDataset, void:entities, NumberOfEntities,
        xsd:integer, VoidGraph)
  ;
    true
  ),
*/

  % void:properties
  count_properties(_, _, VoidDataset, NumberOfProperties),
  rdf_overwrite_datatype(VoidDataset, void:properties, NumberOfProperties,
      xsd:integer, VoidGraph),

  % void:triples
  % Notice that we may have read 0 triples into a graph.
  (   rdf_graph(VoidDataset)
  ->  rdf_statistics(triples_by_graph(VoidDataset, NumberOfTriples))
  ;   NumberOfTriples = 0
  ),
  rdf_overwrite_datatype(VoidDataset, void:triples, NumberOfTriples,
      xsd:integer, VoidGraph).

