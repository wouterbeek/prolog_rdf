:- module(
  rdf_measurement,
  [
    perform_measurement/5 % +Resource:or([bnode,iri])
                          % +Property:iri
                          % :Goal
                          % +Graph:atom
                          % -Measurement:iri
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

:- meta_predicate(perform_measurement(+,+,2,+,-)).

:- rdf_register_prefix(mea, 'http://www.wouterbeek.com/measurement/').



%! perform_measurement(
%!   +Resource:or([bnode,iri]),
%!   +Property:iri,
%!   :Goal,
%!   +Graph:atom,
%!   -Measurement:iri
%! ) is det.

perform_measurement(Resource, Property, Goal, Graph, Measurement):-
  rdf(Property, rdfs:range, Datatype),
  xsd_datatype(Datatype),
  rdf_create_next_resource(measurement, mea, Measurement),
  rdf_assert_indiviudal(Measurement, mea:'Measurement', Graph),
  rdf_assert_now(Measurement, mea:created, Graph),
  call(Property, Resource, Value),
  rdf_assert_datatype_statement(
    Resource,
    Property,
    Value,
    Datatype,
    Graph,
    Measurement
  ).
