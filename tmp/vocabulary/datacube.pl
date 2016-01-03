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
                                       % :Goal_1
                                       % +Graph:rdf_graph
                                       % -Observation:iri
    assert_observation/5, % +Dataset:iri
                          % +Property:iri
                          % :Goal_1
                          % +Graph:rdf_graph
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

:- use_module(library(lists))
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).

:- meta_predicate(assert_observation(+,+,1,+,-)).
:- meta_predicate(assert_multimeasure_observation(+,+,1,+,-)).

:- rdf_meta(assert_dataset(r,+,-)).
:- rdf_meta(assert_datastructure_definition(t,t,t,+,r)).
:- rdf_meta(assert_observation(r,r,:,+,-)).
:- rdf_meta(assert_measure_property(r,r,r,+)).
:- rdf_meta(assert_multimeasure_observation(r,r,:,+,-)).
:- rdf_meta(assert_relation(-,r,+,+)).
:- rdf_meta(assert_relation0(r,+,+,-)).





%! assert_dataset(
%!   +DataStructureDefinition:iri,
%!   +Graph:rdf_graph,
%!   -DataSet:iri
%! ) is det.

assert_dataset(DataStructureDefinition, G, DataSet) :-
  rdf_create_iri(data_set, ['DataSet'], Dataset),
  rdf_assert_instance(Dataset, [qb:'DataSet'], G),
  rdf_assert(DataSet, qb:structure, DataStructureDefinition, G).


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
) :-
  % Create the data structure definition resource if it is not given.
  (   var(DataStructureDefinition)
  ->  rdf_create_iri(
        data_structure_definition,
        ['DataStructureDefinition'],
        DataStructureDefinition
      ),
      rdf_assert_instance(
        DataStructureDefinition,
        qb:'DataStructureDefinition',
        G
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
    rdf_assert(DataStructureDefinition, qb:component, Component, G)
  ).


%! assert_measure_property(
%!   +MeasureProperty:iri,
%!   +Concept:iri,
%!   +Range:iri,
%!   +Graph:rdf_graph
%! ) is det.

assert_measure_property(MeasureProperty, Concept, Range, G) :-
  % rdf:type
  rdf_assert_instance(MeasureProperty, qb:'MeasureProperty', G),

  % qb:concept
  rdf_assert(MeasureProperty, qb:concept, Concept, G),

  % rdfs:range
  rdf_assert(MeasureProperty, rdfs:range, Range, G),

  % rdfs:isDefinedBy
  (   rdf_equal(Prefix:_, MeasureProperty),
      rdf_current_prefix(Prefix, Url)
  ->  rdf_assert(MeasureProperty, rdfs:isDefinedBy, Url, G)
  ;   true
  ).


%! assert_multimeasure_observation(
%!   +Dataset:iri,
%!   +Property:iri,
%!   :Goal_1,
%!   +Graph:rdf_graph,
%!   -Observation:iri
%! ) is det.
% Asserts an observation that belongs to a multi-measure dataset.
% This requires the measurement to be specified explicitly.

assert_multimeasure_observation(
  Dataset,
  Property,
  Goal_1,
  G,
  Observation
) :-
  assert_observation(Dataset, Property, Goal_1, G, Observation),

  % qb:measureType
  rdf_assert(Observation, qb:measureType, Property, G).


%! assert_observation(
%!   +Dataset:iri,
%!   +Property:iri,
%!   :Goal_1,
%!   +Graph:rdf_graph,
%!   -Observation:iri
%! ) is det.

assert_observation(Dataset, Property, Goal_1, G, Observation) :-
  % Extract the datatype.
  rdf(Property, rdfs:range, Datatype),
  rdf_datatype_term(Datatype),

  % Create the observation.
  rdf_create_iri(observation, ['Observation'], Observation),
  rdf_assert_instance(Observation, [qb:'Observation'], G),

  % qb:dataSet
  rdf_assert(Observation, qb:dataSet, Dataset, G),

  % Assert the measurement value.
  call(Goal_1, Value),
  rdf_assert_typed_literal(Observation, Property, Value, Datatype, G),

  % Assert the temporal dimension value.
  rdf_assert_now(Observation, sdmxd:timePeriod, G).


%! assert_slice(+DataSet:iri, +Graph:rdf_graph, -Slice:iri) is det.

assert_slice(DataSet, G, Slice) :-
  rdf_create_iri(slice, ['Slice'], Slice),
  rdf_assert_instance(Slice, [qb:'Slice'], G),
  rdf_assert(DataSet, qb:slice, Slice, G).





% HELPERS %

%! assert_relation(
%!   -Component:or([bnode,iri]),
%!   +Relation:iri,
%!   +Dimension:iri,
%!   +Graph:rdf_graph
%! ) is det.

assert_relation(Component, Relation, Dimension, G) :-
  rdf(Component, Relation, Dimension,  G), !.
assert_relation(Component, Relation, Dimension, G) :-
  rdf_create_bnode(Component),
  rdf_assert(Component, Relation, Dimension, G).
