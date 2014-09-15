:- module(
  datacube,
  [
    assert_observation/4 % +Property:iri
                         % :Goal
                         % +Graph
                         % -Observation:iri
  ]
).

/** <module> RDF measurement

Predicates for perfoming measurements represented in RDF.

@author Wouter Beek
@version 2014/09
*/

:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(rdf_build)).
:- use_module(plRdf(rdf_dateTime)).
:- use_module(plRdf(rdfs_build)).
:- use_module(plRdf_rei(rdf_reification_write)).

:- use_module(plXsd(xsd)).

:- meta_predicate(assert_observation(+,+,2,+,-)).

:- rdf_register_prefix(dct, 'http://purl.org/dc/terms/').
:- rdf_register_prefix(mea, 'http://www.wouterbeek.com/measurement/').
:- rdf_register_prefix(qb, 'http://purl.org/linked-data/cube#').



%! assert_observation(
%!   +Property:iri,
%!   :Goal,
%!   +Graph:atom,
%!   -Observation:iri
%! ) is det.

assert_observation(Property, Goal, Graph, Observation):-
  % Extract the datatype.
  rdf(Property, rdfs:range, Datatype),
  xsd_datatype(Datatype),
  
  % Create the observation.
  rdf_create_next_resource(observation, qb, Observation),
  rdf_assert_indiviudal(Observation, qb:'Observation', Graph),
  
  % Assert the property as a measure property.
  rdf_assert_instance(Property, qb:'MeasureProperty', Graph),
  
  % Assert the measurement value.
  call(Property, Resource, Value),
  rdf_assert_datatype(Observation, Property, Value, Datatype, Graph).

