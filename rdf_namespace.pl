:- module(
  rdf_namespace,
  [
    rdf_convert_namespace/6, % +FromNamespace:atom
                             % +ToNamespace:atom
                             % ?Subject:or([bnode,iri])
                             % ?Predicate:iri
                             % ?Object:or([bnode,literal,iri])
                             % ?Graph:atom
    rdf_namespaces/2, % +Graph:atom
                      % -XmlNamespaces:ordset(atom)
    rdf_resource_to_namespace/3 % +Resource:uri
                                % -Namespace:atom
                                % -Name:atom
  ]
).

/** <module> RDF namespaces

Namespace support for RDF(S), building on namespace prefix support for XML.

@author Wouter Beek
@version 2013/03-2013/05, 2014/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf_term(rdf_term)).



%! rdf_convert_namespace(
%!   +FromNamespace:atom,
%!   +FromResource:or([bnode,iri,literal]),
%!   +ToNamespace:atom,
%!   +ToResource:or([bnode,iri,literal])
%! ) is det.

rdf_convert_namespace(_, _, BNode, BNode):-
  rdf_is_bnode(BNode), !.
rdf_convert_namespace(_, _, Literal, Literal):-
  rdf_is_literal(Literal), !.
rdf_convert_namespace(FromNS, ToNS, FromIRI, ToIRI):-
  rdf_global_id(FromNS:LocalName, FromIRI),
  rdf_global_id(ToNS:LocalName, ToIRI).

%! rdf_convert_namespace(
%!   +FromNamespace:atom,
%!   +ToNamespace:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?Graph:atom
%! ) is det.
% Converts all resources that occur in the given patterns
% with the given namespace to similar resources that have another namespace.
%
% The namespaces must be registered with module [xml_namespace].

:- rdf_meta(rdf_convert_namespace(+,+,r,r,r,?)).
rdf_convert_namespace(FromNS, ToNs, S1, P1, O1, G):-
  forall(
    rdf_retractall(S1, P1, O1, G),
    (
      maplist(rdf_convert_namespace(FromNS, ToNs), [S1,P1,O1], [S2,P2,O2]),
      rdf_assert(S2, P2, O2, G)
    )
  ).


rdf_namespaces(G, XmlNamespaces):-
  aggregate_all(
    set(XmlNamespace),
    (
      rdf_term(Term, G),
      rdf_global_id(XmlNamespace:_, Term)
    ),
    XmlNamespaces
  ).


%! rdf_resource_to_namespace(
%!   +Resource:uri,
%!   -Namespace:atom,
%!   -Name:atom
%! ) is det.

rdf_resource_to_namespace(Resource, LongestNamespace, ShortestLocalName):-
  findall(
    LocalNameLength-Namespace,
    (
      rdf_global_id(Namespace:LocalName, Resource),
      atom_length(LocalName, LocalNameLength)
    ),
    Pairs
  ),
  keysort(Pairs, [_-LongestNamespace|_]),
  rdf_global_id(LongestNamespace:ShortestLocalName, Resource).

