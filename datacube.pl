:- module(
  datacube,
  [
    assert_datastructure_definition/5, % +Dimensions:list(iri)
                                       % +Measure:iri
                                       % +Attributes:list(iri)
                                       % +Graph:atom
                                       % -DataStructureDefinition:iri
    assert_measure_property/4, % +MeasureProperty:iri
                               % +Concept:iri
                               % +Range:iri
                               % +Graph:atom
    assert_observation/4, % +Dataset:iri
                          % +Property:iri
                          % :Goal
                          % +Graph
    assert_observation/5 % +Dataset:iri
                         % +Property:iri
                         % :Goal
                         % +Graph
                         % -Observation:iri
  ]
).

/** <module> RDF measurement

Predicates for perfoming measurements represented in RDF.

@author Wouter Beek
@version 2014/09-2014/10
*/

:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdf_prefixes)). % RDF prefix declarations.
:- use_module(plRdf_term(rdf_datatype)).
:- use_module(plRdf_term(rdf_dateTime)).

:- use_module(plXsd(xsd)).

:- meta_predicate(assert_observation(+,+,1,+)).
:- meta_predicate(assert_observation(+,+,1,+,-)).

:- rdf_meta(assert_datastructure_definition(t,r,t,+,-)).
:- rdf_meta(assert_observation(r,r,:,+)).
:- rdf_meta(assert_observation(r,r,:,+,-)).
:- rdf_meta(assert_relation(-,r,+,+)).
:- rdf_meta(assert_relation0(r,+,+,-)).
:- rdf_meta(rdf_assert0(r,r,+,o)).



%! assert_datastructure_definition(
%!   +Dimensions:list(iri),
%!   +Measure:iri,
%!   +Attributes:list(iri),
%!   +Graph:atom,
%!   -DataStructureDefinition:iri
%! ) is det.
% @tbd Add support for qb:order.
% @tbd Add support for qb:componentRequired.
% @tbd Fix RDF expansion in lambda expression.

assert_datastructure_definition(
  Dimensions,
  Measure,
  Attributes,
  Graph,
  DSDef
):-
  % Create the data structure definition resource.
  rdf_create_next_resource(
    data_structure_definition,
    Graph,
    ['DataStructureDefinition'],
    DSDef
  ),
  rdf_assert_instance(DSDef, qb:'DataStructureDefinition', Graph),

  % Create the component resources.
  findall(
    Component,
    (
      member(Dimension, Dimensions),
      assert_relation(Component, qb:dimension, Dimension, Graph)
    ),
    ComponentsA
  ),
  assert_relation(ComponentB, qb:measure, Measure, Graph),
  findall(
    Component,
    (
      member(Attribute, Attributes),
      assert_relation(Component, qb:attribute, Attribute, Graph)
    ),
    ComponentsC
  ),
  append([ComponentB|ComponentsA], ComponentsC, Components),

  % Relate components to data structure definition.
  forall(
    member(Component, Components),
    rdf_assert(DSDef, qb:component, Component, Graph)
  ).


%! assert_measure_property(
%!   +MeasureProperty:iri,
%!   +Concept:iri,
%!   +Range:iri,
%!   +Graph:atom
%! ) is det.

assert_measure_property(MeasureProperty, Concept, Range, Graph):-
  % rdf:type
  rdf_assert_individual(MeasureProperty, qb:'MeasureProperty', Graph),
  
  % qb:concept
  rdf_assert(MeasureProperty, qb:concept, Concept, Graph),
  
  % rdfs:range
  rdfs_assert(MeasureProperty, rdfs:range, xsd:float, Graph),
  
  % rdfs:isDefinedBy
  (   rdf_global_id(Prefix:_, MeasureProperty),
      rdf_current_prefix(Prefix, Url)
  ->  rdf_assert(MeasureProperty, rdfs:isDefinedBy, Url, Graph)
  ;   true
  ).


%! assert_observation(+Dataset:iri, +Property:iri, :Goal, +Graph:atom) is det.
% @see assert_observation/5

assert_observation(D, P, Goal, G):-
  assert_observation(D, P, Goal, G, _).


%! assert_observation(
%!   +Dataset:iri,
%!   +Property:iri,
%!   :Goal,
%!   +Graph:atom,
%!   -Observation:iri
%! ) is det.

assert_observation(Dataset, Property, Goal, Graph, Observation):-
  % Extract the datatype.
  rdf(Property, rdfs:range, Datatype),
  xsd_datatype(Datatype),

  % Create the observation.
  rdf_create_next_resource(observation, Graph, ['Observation'], Observation),
  rdf_assert_instance(Observation, qb:'Observation', Graph),

  % qb:dataSet
  rdf_assert(Observation, qb:dataSet, Dataset, Graph),

  % Assert the measurement value.
  call(Goal, Value),
  rdf_assert_datatype(Observation, Property, Value, Datatype, Graph),

  % Assert the temporal dimension value.
  rdf_assert_now(Observation, 'sdmx-dimension':timePeriod, Graph).



% Helpers

%! assert_relation(
%!   -Component:or([bnode,iri]),
%!   +Relation:iri,
%!   +Dimension:iri,
%!   +Graph:atom
%! ) is det.

assert_relation(Component, Relation, Dimension, Graph):-
  rdf(Component, Relation, Dimension,  Graph), !.
assert_relation(Component, Relation, Dimension, Graph):-
  rdf_bnode(Component),
  rdf_assert(Component, Relation, Dimension, Graph).

