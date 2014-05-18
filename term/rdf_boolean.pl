:- module(
  rdf_boolean,
  [
    rdf_assert_false/3, % +Subject:or([bnode,iri])
                        % +Predicate:iri
                        % +RdfGraph:atom
    rdf_assert_true/3, % +Subject:or([bnode,iri])
                       % +Predicate:iri
                       % +RdfGraph:atom
    rdf_false/3, % ?Subjet:or([bnode,iri])
                 % ?Predicate:iri
                 % ?RdfGraph:atom
    rdf_true/3 % ?Subjet:or([bnode,iri])
               % ?Predicate:iri
               % ?RdfGraph:atom
  ]
).

/** <module> RDF boolean

Support for RDF triples with a literal object term
with datatype IRI xsd:boolean.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_datatype)).
:- use_module(xsd(xsd)). % XML namespace.

:- rdf_meta(rdf_assert_false(r,r,+)).
:- rdf_meta(rdf_assert_true(r,r,+)).
:- rdf_meta(rdf_false(r,r,?)).
:- rdf_meta(rdf_true(r,r,?)).



%! rdf_assert_false(+Subject:or([bnode,iri]), +Predicate:iri, +RdfGraph:atom) is det.

rdf_assert_false(S, P, G):-
  rdf_assert_datatype(S, P, false, xsd:boolean, G).


%! rdf_assert_true(+Subject:or([bnode,iri]), +Predicate:iri, +RdfGraph:atom) is det.

rdf_assert_true(S, P, G):-
  rdf_assert_datatype(S, P, true, xsd:boolean, G).


%! rdf_false(?Subject:or([bnode,iri]), ?Predicate:iri, ?RdfGrapg:atom) is nondet.

rdf_false(S, P, G):-
  rdf_datatype(S, P, false, xsd:boolean, G).


%! rdf_true(?Subject:or([bnode,iri]), ?Predicate:iri, ?RdfGrapg:atom) is nondet.

rdf_true(S, P, G):-
  rdf_datatype(S, P, true, xsd:boolean, G).

