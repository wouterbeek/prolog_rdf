:- module(
  rdf_datetime,
  [
    rdf_assert_now/3, % +Term:rdf_term
                      % +Predicate:iri
                      % ?Graph:atom
    rdf_assert_today/3 % +Term:rdf_term
                       % +Predicate:iri
                       % ?Graph:atom
  ]
).

/** <module> RDF date-time

Support for RDF triples with a literal object term
 denoting a commonly occurring date-time value.

@author Wouter Beek
@version 2014/03, 2014/09, 2014/11-2014/12
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plXsd(dateTime/xsd_dateTime_functions)).

:- use_module(plRdf(api/rdf_build)).

:- rdf_meta(rdf_assert_now(o,r,?)).
:- rdf_meta(rdf_assert_today(o,r,?)).





%! rdf_assert_now(+Term:rdf_term, +Predicate:iri, ?Graph:atom) is det.

rdf_assert_now(Term, P, Graph):-
  get_time(Timestamp),
  posix_timestamp_to_xsd_dateTime(Timestamp, Datetime),
  rdf_assert_typed_literal(Term, P, Datetime, xsd:dateTime, Graph).



%! rdf_assert_today(+Term:rdf_term, +Predicate:iri, ?Graph:atom) is det.

rdf_assert_today(Term, P, Graph):-
  get_time(Timestamp),
  posix_timestamp_to_xsd_dateTime(Timestamp, Datetime),
  rdf_assert_typed_literal(Term, P, Datetime, xsd:date, Graph).
