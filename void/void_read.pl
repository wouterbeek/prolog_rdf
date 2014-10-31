:- module(
  void_read,
  [
    rdf_link/5, % ?Subject:oneof([bnode,uri])
                % ?Predicate:uri
                % ?Object:oneof([bnode,literal,uri])
                % ?FromGraph:atom
                % ?ToGraph:atom
    rdf_linkset/3 % +Triples:list(compound)
                  % ?FromGraph:atom
                  % ?ToGraph:atom
  ]
).

/** <module> VoID read

Read information for VoID descriptions.

Also contains predicates for the definition of RDF links and RDF linksets
as it appears in the VoID specification.

@author Wouter Beek
@version 2013/10, 2014/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(plRdf_term(rdf_term)).

:- rdf_meta(rdf_link(r,r,o,?,?)).
:- rdf_meta(rdf_linkset(t,?,?)).



%! rdf_link(
%!   ?Term1:or([bnode,iri,literal]),
%!   ?Graph1:atom,
%!   ?Predicate:iri,
%!   ?Term2:or([bnode,iri,literal]),
%!   ?Graph2:atom
%! ) is nondet.
% An RDF link is an RDF triple whose subject and object
%  are described in different datasets.
%
% @tbd Take literal2bnode map into account?

rdf_link(T1, G1, P, T2, G2):-
  rdf(T1, P, T2),
  rdf_term(T1, G1),
  rdf_term(T2, G2),
  G1 \== G2.


%! rdf_linkset(+Triples:list(compound), ?FromGraph:atom, ?ToGraph:atom) is semidet.
%! rdf_linkset(-Triples:list(compound), +FromGraph:atom, +ToGraph:atom) is det
% An RDF linkset is a collection of RDF links between the same two datasets.

rdf_linkset(Ts, FromG, ToG):-
  nonvar(Ts), !,
  forall(
    member(row(S,P,O), Ts),
    rdf_link(S, P, O, FromG, ToG)
  ).
rdf_linkset(Ts, FromG, ToG):-
  findall(
    row(S, P, O),
    rdf_link(S, P, O, FromG, ToG),
    Ts
  ).

