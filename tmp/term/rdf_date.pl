:- module(
  rdf_date,
  [
    rdf_assert_now/3, % +Subject:or([bnode,iri])
                      % +Predicate:iri
                      % ?Graph:atom
    rdf_assert_today/3 % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % ?Graph:atom
  ]
).

/** <module> RDF date-time

Support for RDF triples with a literal object term
 denoting a commonly occurring date-time value.

@author Wouter Beek
@version 2014-2015
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(os/date_ext)).

:- use_module(plRdf(api/rdf_build)).

:- rdf_meta(rdf_assert_date(r,r,r,?)).
:- rdf_meta(rdf_assert_now(r,r,?)).
:- rdf_meta(rdf_assert_today(r,r,?)).





%! rdf_assert_date(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Datatype:iri,
%!   ?Graph:atom
%! ) is det.

rdf_assert_date(S, P, D, G):-
  get_date(Date),
  rdf_assert_typed_literal(S, P, Date, D, G).



%! rdf_assert_now(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   ?Graph:atom
%! ) is det.

rdf_assert_now(S, P, G):-
  rdf_assert_date(S, P, xsd:dateTime, G).



%! rdf_assert_today(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   ?Graph:atom
%! ) is det.

rdf_assert_today(S, P, G):-
  rdf_assert_date(S, P, xsd:date, G).
