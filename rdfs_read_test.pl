:- module(rdfs_read_test, []).

:- use_module(library(plunit)).

:- begin_tests(rdfs_read).

:- use_module(library(semweb/rdf_db)). % rdf_meta/1
:- use_module(rdfs(rdfs_read)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- xml_register_namespace(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').

:- rdf_meta(test_triple(?,r,r,o)).

test(rdf_query, [forall(test_triple(M, S, P, O))]):-
  rdf_global_id(rdf:type, P),
  rdfs_individual(M, S, O, _).

%! test_triple(
%!   ?Mode:compound,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri])
%! ) is nondet.

test_triple(m(t,f,f), rdfs:'Resource',    rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdfs:'Class',       rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdfs:'Literal',     rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdf:'XMLLiteral',   rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdfs:'Datatype',    rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdf:'Seq',          rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdf:'Bag',          rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdf:'Alt',          rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdfs:'Container',   rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdf:'List',         rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdfs:'ContainerMembershipProperty', rdf:type, rdfs:'Class').
test_triple(m(t,f,f), rdf:'Property',     rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdf:'Statement',    rdf:type, rdfs:'Class'  ).
test_triple(m(t,f,f), rdfs:domain,        rdf:type, rdf:'Property').
test_triple(m(t,f,f), rdfs:range,         rdf:type, rdf:'Property').
test_triple(m(t,f,f), rdfs:subPropertyOf, rdf:type, rdf:'Property').
test_triple(m(t,f,f), rdfs:subClassOf,    rdf:type, rdf:'Property').
test_triple(m(t,f,f), rdfs:member,        rdf:type, rdf:'Property').
test_triple(m(t,f,f), rdfs:seeAlso,       rdf:type, rdf:'Property').
test_triple(m(t,f,f), rdfs:isDefinedBy,   rdf:type, rdf:'Property').
test_triple(m(t,f,f), rdfs:comment,       rdf:type, rdf:'Property').
test_triple(m(t,f,f), rdfs:label,         rdf:type, rdf:'Property').

:- end_tests(rdfs_read).

