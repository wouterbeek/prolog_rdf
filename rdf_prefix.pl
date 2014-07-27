:- module(
  rdf_namespace,
  [
    rdf_convert_prefixes/6, % +FromPrefix:atom
                            % +ToPrefix:atom
                            % ?Subject:or([bnode,iri])
                            % ?Predicate:iri
                            % ?Object:or([bnode,iri,literal])
                            % ?Graph:atom
    rdf_prefixes/1, % -Prefixes:ordset(atom)
    rdf_prefixes/2, % ?Graph:atom
                    % -Prefixes:ordset(atom)
    rdf_iri_to_prefix/3 % +Iri:iri
                        % -LongestPrefix:atom
                        % -ShortestLocalName:atom
  ]
).

/** <module> RDF prefixes

Namespace support for RDF(S), building on namespace prefix support for XML.

@author Wouter Beek
@version 2013/03-2013/05, 2014/01, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf_term(rdf_term)).

:- rdf_meta(rdf_convert_prefixes(+,+,r,r,r,?)).



%! rdf_convert_prefixes(
%!   +FromPrefix:atom,
%!   +FromTerm:or([bnode,iri,literal]),
%!   +ToPrefix:atom,
%!   +ToTerm:or([bnode,iri,literal])
%! ) is det.

rdf_convert_prefixes(_, _, BNode, BNode):-
  rdf_is_bnode(BNode), !.
rdf_convert_prefixes(_, _, Literal, Literal):-
  rdf_is_literal(Literal), !.
rdf_convert_prefixes(FromPrefix, ToPrefix, FromIri, ToIri):-
  rdf_global_id(FromPrefix:LocalName, FromIri),
  rdf_global_id(ToPrefix:LocalName, ToIri).

%! rdf_convert_prefixes(
%!   +FromPrefix:atom,
%!   +ToPrefix:atom,
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:or([bnode,literal,iri]),
%!   ?Graph:atom
%! ) is det.
% Converts all resources that occur in the given patterns
% with the given namespace to similar resources that have another namespace.
%
% The namespaces must be registered with module [xml_namespace].

rdf_convert_prefixes(FromPrefix, ToPrefix, S1, P1, O1, Graph):-
  forall(
    rdf_retractall(S1, P1, O1, Graph),
    (
      maplist(
        rdf_convert_prefixes(FromPrefix, ToPrefix),
        [S1,P1,O1],
        [S2,P2,O2]
      ),
      rdf_assert(S2, P2, O2, Graph)
    )
  ).


%! rdf_prefixes(-Prefixes:ordset(atom)) is det.

rdf_prefixes(Prefixes):-
  aggregate_all(
    set(Prefix),
    rdf_current_prefix(Prefix, _),
    Prefixes
  ).

%! rdf_prefixes(+Graph:atom, -Prefixes:ordset(atom)) is det.

rdf_prefixes(Graph, Prefixes):-
  var(Graph), !,
  rdf_prefixes(Prefixes).
rdf_prefixes(Graph, Prefixes):-
  aggregate_all(
    set(Prefix),
    (
      rdf_iri(Iri, Graph),
      rdf_global_id(Prefix:_, Iri)
    ),
    Prefixes
  ).


%! rdf_iri_to_prefix(
%!   +Iri:iri,
%!   -LongestPrefix:atom,
%!   -ShortestLocalName:atom
%! ) is det.

rdf_iri_to_prefix(Resource, LongestPrefix, ShortestLocalName):-
  findall(
    LocalNameLength-Prefix,
    (
      rdf_global_id(Prefix:LocalName, Iri),
      atom_length(LocalName, LocalNameLength)
    ),
    Pairs
  ),
  keysort(Pairs, [_-LongestPrefix|_]),
  rdf_global_id(LongestPrefix:ShortestLocalName, Iri).

