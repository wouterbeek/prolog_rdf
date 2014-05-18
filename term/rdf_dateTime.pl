:- module(
  rdf_dateTime,
  [
    rdf_assert_now/3, % +Subject:or([bnode,iri])
                      % +Predicate:iri
                      % +RdfGraph:atom
    rdf_assert_today/3, % +Subject:or([bnode,iri])
                        % +Predicate:iri
                        % +RdfGraph:atom
    rdf_update_now/3, % +Subject:or([bnode,iri])
                      % +Predicate:iri
                      % +RdfGraph:atom
    rdf_update_today/3 % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % +RdfGraph:atom
  ]
).

/** <module> RDF string

Support for RDF triples with a literal object term
with datatype IRI xsd:string.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(xsd(xsd_dateTime_ext)).

:- rdf_meta(rdf_assert_now(r,r,+)).
:- rdf_meta(rdf_assert_today(r,r,+)).
:- rdf_meta(rdf_update_now(r,r,+)).
:- rdf_meta(rdf_update_today(r,r,+)).



%! rdf_assert_now(+Subject:or([bnode,iri]), +Predicate:iri, +RdfGraph:atom) is det.

rdf_assert_now(S, P, G):-
  get_time(POSIX_TS),
  posix_timestamp_to_xsd_dateTime(POSIX_TS, XSD_DT),
  rdf_assert_datatype(S, P, XSD_DT, xsd:dateTime, G).


%! rdf_assert_today(+Subject:or([bnode,iri]), +Predicate:iri, +RdfGraph:atom) is det.

rdf_assert_today(S, P, G):-
  get_time(POSIX_TS),
  posix_timestamp_to_xsd_dateTime(POSIX_TS, XSD_DT),
  rdf_assert_datatype(S, P, XSD_DT, xsd:date, G).


%! rdf_update_now(+Subject:or([bnode,iri]), +Predicate:iri, +RdfGraph:atom) is det.

rdf_update_now(S, P, G):-
  rdf_retractall_datatype(S, P, xsd:dateTime, G),
  rdf_assert_now(S, P, G).


%! rdf_update_today(+Subject:or([bnode,iri]), +Predicate:iri, +RdfGraph:atom) is det.

rdf_update_today(S, P, G):-
  rdf_retractall_datatype(S, P, xsd:date, G),
  rdf_assert_today(S, P, G).

