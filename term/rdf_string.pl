:- module(
  rdf_string,
  [
    rdf_assert_string/4, % +Subject:or([bnode,iri])
                         % +Predicate:iri
                         % +String:or([atom,list(code),string])
                         % +RdfGraph:atom
    rdf_retractall_string/3, % ?Subject:or([bnode,iri])
                             % ?Predicate:iri
                             % ?RdfGraph:atom
    rdf_retractall_string/4, % ?Subject:or([bnode,iri])
                             % ?Predicate:iri
                             % ?String:or([atom,list(code),string])
                             % ?RdfGraph:atom
    rdf_string/4 % ?Subject:or([bnode,iri])
                 % ?Predicate:iri
                 % ?String:or([atom,list(code),string])
                 % ?RdfGraph:atom
  ]
).

/** <module> RDF string

Support for RDF triples with a literal object term
with datatype IRI xsd:string.

@author Wouter Beek
@version 2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_literal)).
:- use_module(rdf_term(rdf_literal_build)).

:- rdf_meta(rdf_assert_string(r,r,+,+)).
:- rdf_meta(rdf_retractall_string(r,r,?,?)).
:- rdf_meta(rdf_string(r,r,?,?)).



%! rdf_assert_string(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +String:or([atom,list(code),string]),
%!   +RdfGraph:atom
%! ) is det.

rdf_assert_string(S, P, String, G):-
  % @tbd ...
  rdf_global_id(xsd:string, Datatype),
  rdf_assert_literal(S, P, String, Datatype, G).


%! rdf_retractall_string(?Subject:or([bnode,iri]), ?Predicate:iri, ?RdfGraph:atom) is det.
%! rdf_retractall_string(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?String:or([atom,list(code),string]),
%!   ?RdfGraph:atom
%! ) is det.

rdf_retractall_string(S, P, G):-
  rdf_retractall_string(S, P, _, G).
rdf_retractall_string(S, P, String, G):-
  rdf_retractall_literal(S, P, String, xsd:string, G).


%! rdf_string(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?String:or([atom,list(code),string]),
%!   ?RdfGraph:atom
%! ) is nondet.

rdf_string(S, P, String, G):-
  rdf_literal(S, P, String, xsd:string, _, G).

