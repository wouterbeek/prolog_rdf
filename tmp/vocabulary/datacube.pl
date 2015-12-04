:- module(
  datacube,
  [
    assert_dataset/3, % +DataStructureDefinition:iri
                      % +Graph:rdf_graph
                      % -DataSet:iri
    assert_datastructure_definition/5, % +Dimensions:list(iri)
                                       % +Measures:list(iri)
                                       % +Attributes:list(iri)
                                       % +Graph:rdf_graph
                                       % ?DataStructureDefinition:or([bnode,iri])
    assert_measure_property/4, % +MeasureProperty:iri
                               % +Concept:iri
                               % +Range:iri
                               % +Graph:rdf_graph
    assert_multimeasure_observation/5, % +Dataset:iri
                                       % +Property:iri
                                       % :Goal
                                       % +Graph
                                       % -Observation:iri
    assert_observation/5, % +Dataset:iri
                          % +Property:iri
                          % :Goal
                          % +Graph
                          % -Observation:iri
    assert_slice/3 % +DataSet:iri
                   % +Graph:rdf_graph
                   % -Slice:iri
  ]
).

/** <module> RDF measurement

Predicates for perfoming measurements represented in RDF.

@author Wouter Beek
@version 2014/09-2014/11, 2015/01, 2015/12
*/

:- use_module(library(lists)).
:- use_module(library(rdf/rdf_api)).

:- meta_predicate(assert_observation(+,+,1,+,-)).
:- meta_predicate(assert_multimeasure_observation(+,+,1,+,-)).

:- rdf_meta(assert_dataset(r,+,-)).
:- rdf_meta(assert_datastructure_definition(t,t,t,+,r)).
:- rdf_meta(assert_observation(r,r,:,+,-)).
:- rdf_meta(assert_measure_property(r,r,r,+)).
:- rdf_meta(assert_multimeasure_observation(r,r,:,+,-)).
:- rdf_meta(assert_relation(-,r,+,+)).
:- rdf_meta(assert_relation0(r,+,+,-)).
:- rdf_meta(rdf_assert0(r,r,+,o)).

:- rdf_register_prefix(qb, 'http://purl.org/linked-data/cube#').
:- rdf_register_prefix(
     'sdmx-dimension',
     'http://purl.org/linked-data/sdmx/2009/dimension#'
   ).





%! assert_dataset(
%!   +DataStructureDefinition:iri,
%!   +Graph:rdf_graph,
%!   -DataSet:iri
%! ) is det.

assert_dataset(DataStructureDefinition, G, DataSet):-
  rdf_create_next_resource(
    data_set,
    ['DataSet'],
    qb:'DataSet',
    G,
    DataSet
  ),
  user:rdf_assert(DataSet, qb:structure, DataStructureDefinition, G).


%! assert_datastructure_definition(
%!   +Dimensions:list(iri),
%!   +Measures:list(iri),
%!   +Attributes:list(iri),
%!   +Graph:rdf_graph,
%!   ?DataStructureDefinition:iri
%! ) is det.
% @tbd Add support for qb:order.
% @tbd Add support for qb:componentRequired.
% @tbd Fix RDF expansion in lambda expression.

assert_datastructure_definition(
  Dimensions,
  Measures,
  Attributes,
  G,
  DataStructureDefinition
):-
  % Create the data structure definition resource if it is not given.
  (   var(DataStructureDefinition)
  ->  rdf_create_next_resource(
        data_structure_definition,
        ['DataStructureDefinition'],
        qb:'DataStructureDefinition',
        G,
        DataStructureDefinition
      )
  ;   true
  ),
  
  % Create the component resources.
  % qb:dimension
  findall(
    Component,
    (
      member(Dimension, Dimensions),
      assert_relation(Component, qb:dimension, Dimension, G)
    ),
    ComponentsA
  ),
  % qb:measure
  findall(
    Component,
    (
      member(Measure, Measures),
      assert_relation(Component, qb:measure, Measure, G)
    ),
    ComponentsB
  ),
  % qb:attribute
  findall(
    Component,
    (
      member(Attribute, Attributes),
      assert_relation(Component, qb:attribute, Attribute, G)
    ),
    ComponentsC
  ),
  append([ComponentsA,ComponentsB,ComponentsC], Components),

  % Relate components to data structure definition.
  % qb:component
  forall(
    member(Component, Components),
    user:rdf_assert(DataStructureDefinition, qb:component, Component, G)
  ).


%! assert_measure_property(
%!   +MeasureProperty:iri,
%!   +Concept:iri,
%!   +Range:iri,
%!   +Graph:rdf_graph
%! ) is det.

assert_measure_property(MeasureProperty, Concept, Range, G):-
  % rdf:type
  rdf_assert_instance(MeasureProperty, qb:'MeasureProperty', G),

  % qb:concept
  user:rdf_assert(MeasureProperty, qb:concept, Concept, G),

  % rdfs:range
  user:rdf_assert(MeasureProperty, rdfs:range, Range, G),

  % rdfs:isDefinedBy
  (   rdf_expand_ct(Prefix:_, MeasureProperty),
      rdf_current_prefix(Prefix, Url)
  ->  rdf_assert(MeasureProperty, rdfs:isDefinedBy, Url, G)
  ;   true
  ).


%! assert_multimeasure_observation(
%!   +Dataset:iri,
%!   +Property:iri,
%!   :Goal,
%!   +Graph:rdf_graph,
%!   -Observation:iri
%! ) is det.
% Asserts an observation that belongs to a multi-measure dataset.
% This requires the measurement to be specified explicitly.

assert_multimeasure_observation(
  Dataset,
  Property,
  Goal,
  G,
  Observation
):-
  assert_observation(Dataset, Property, Goal, G, Observation),

  % qb:measureType
  user:rdf_assert(Observation, qb:measureType, Property, G).


%! assert_observation(
%!   +Dataset:iri,
%!   +Property:iri,
%!   :Goal,
%!   +Graph:rdf_graph,
%!   -Observation:iri
%! ) is det.

assert_observation(Dataset, Property, Goal, G, Observation):-
  % Extract the datatype.
  user:rdf(Property, rdfs:range, Datatype),
  rdf_datatype_term(Datatype),

  % Create the observation.
  rdf_create_next_resource(
    observation,
    ['Observation'],
    qb:'Observation',
    G,
    Observation
  ),

  % qb:dataSet
  user:rdf_assert(Observation, qb:dataSet, Dataset, G),

  % Assert the measurement value.
  call(Goal, Value),
  rdf_assert_typed_literal(Observation, Property, Value, Datatype, G),

  % Assert the temporal dimension value.
  rdf_assert_now(Observation, 'sdmx-dimension':timePeriod, G).


%! assert_slice(+DataSet:iri, +Graph:rdf_graph, -Slice:iri) is det.

assert_slice(DataSet, G, Slice):-
  rdf_create_next_resource(slice, ['Slice'], qb:'Slice', G, Slice),
  user:rdf_assert(DataSet, qb:slice, Slice, G).





% HELPERS %

%! assert_relation(
%!   -Component:or([bnode,iri]),
%!   +Relation:iri,
%!   +Dimension:iri,
%!   +Graph:rdf_graph
%! ) is det.

assert_relation(Component, Relation, Dimension, G):-
  rdf(Component, Relation, Dimension,  G), !.
assert_relation(Component, Relation, Dimension, G):-
  rdf_create_bnode(Component),
  rdf_assert(Component, Relation, Dimension, G).
