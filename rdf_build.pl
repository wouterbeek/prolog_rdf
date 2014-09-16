:- module(
  rdf_build,
  [
    rdf_assert_instance/3, % +Instance:iri
                           % +Class:iri
                           % +Graph:atom
    rdf_assert_property/2, % +Property:iri
                           % +Graph:atom
    rdf_copy/5, % +FromGraph:atom
                % ?Subject:or([bnode,iri])
                % ?Predicate:iri
                % ?Object:or([bnode,iri,literal])
                % +ToGraph:atom
    rdf_create_next_resource/4, % +Flag:atom
                                % +Prefix:atom
                                % +SubPaths:list(atom)
                                % -Resource:iri
    rdf_remove_property/2, % +Graph:atom
                           % +Property:iri
    rdf_remove_resource/2 % +Graph:atom
                          % +Resource:iri
  ]
).

/** <module> RDF build

Simple asserion and retraction predicates for RDF.
Triples with literals are treated in dedicated modules.

@author Wouter Beek
@version 2013/10, 2013/12-2014/01, 2014/06, 2014/08
*/

:- use_module(library(semweb/rdf_db)).


:- use_module(plRdf(rdf_read)).

:- rdf_meta(rdf_assert_instance(r,r,+)).
:- rdf_meta(rdf_assert_property(r,+)).
:- rdf_meta(rdf_remove_property(+,r)).
:- rdf_meta(rdf_remove_resource(+,r)).



%! rdf_assert_instance(+Instance:iri, +Class:iri, +Graph:graph) is det.
% Asserts an instance/class relationship.

rdf_assert_instance(I, C, G):-
  rdf_assert(I, rdf:type, C, G),
  rdf_assert(C, rdf:type, rdfs:'Class', G).


rdf_assert_property(Property, G):-
  rdf_assert_instance(Property, rdf:'Property', G).


%! rdf_create_next_resource(
%!   +Flag:atom,
%!   +Prefix:atom,
%!   +SubPaths:list(atom),
%!   -Resource:iri
%! ) is det.

rdf_create_next_resource(Flag, Prefix1, SubPaths1, Resource):-
gtrace,
  flag(Flag, Id, Id + 1),
  append(SubPaths1, [Id], SubPaths2),
  atomic_list_concat(SubPaths2, '/', Path),
  rdf_global_id(Prefix1:'', Prefix2),
  uri_normalized(Path, Prefix2, Resource).

rdf_graph(G1, S, P, O, G2):-
  forall(
    rdf(S, P, O, G1),
    rdf_assert(S, P, O, G2)
  ).


%! rdf_copy(
%!   +FromGraph:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,iri,literal]),
%!   +ToGraph:atom
%! ) is det.
% Copies triples between graphs.
%
% @tbd Perform blank node renaming.

rdf_copy(FromGraph, S, P, O, ToGraph):-
  forall(
    rdf(S, P, O, FromGraph),
    rdf_assert(S, P, O, ToGraph)
  ).


rdf_remove_property(G, P):-
  rdf_property(G, P), !,
  rdf_remove_resource(G, P).

rdf_remove_resource(G, R):-
  rdf_retractall(R, _, _, G),
  rdf_retractall(_, R, _, G),
  rdf_retractall(_, _, R, G).

