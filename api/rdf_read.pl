:- module(
  rdf_read,
  [
    rdf_ground_triple/4, % ?Subject:or([bnode,iri])
                         % ?Predicate:iri
                         % ?Object:rdf_term
                         % ?Graph:atom
    rdf_id/2, % ?Term:rdf_term
              % ?EquivTerm:rdf_term
    rdf_is_ground_triple/1, % +Triple:compound
    rdf_resource_edge/4, % +Term:rdf_term
                         % -Predicate:iri
                         % -OtherTerm:rdf_term
                         % ?Graph:atom
    rdf_resource_incoming_edge/4, % +Object:rdf_term
                                  % -Predicate:iri
                                  % -Subject:or([bnode,iri])
                                  % ?Graph:atom
    rdf_resource_outgoing_edge/4 % +Subject:or([bnode,iri])
                                 % -Predicate:iri
                                 % -Object:rdf_term
                                 % ?Graph:atom
    rdf_term_edge/4, % +Term:rdf_term
                     % -Predicate:iri
                     % -OtherTerm:rdf_term
                     % ?Graph:atom
    rdf_term_incoming_edge/4, % +Object:rdf_term
                              % -Predicate:iri
                              % -Subject:or([bnode,iri])
                              % ?Graph:atom
    rdf_term_outgoing_edge/4, % +Subject:or([bnode,iri])
                              % -Predicate:iri
                              % -Object:rdf_term
                              % ?Graph:atom
    rdf_triples/2 % +Graph:atom
                  % -Triples:ordset(compound)
  ]
).

/** <module> RDF API: Read

Predicates for reading from RDF, customized for specific datatypes and
 literals.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- use_module(plRdf(term/rdf_term)).

:- rdf_meta(rdf_ground_triple(r,r,o,?)).
:- rdf_meta(rdf_id(o,o)).
:- rdf_meta(rdf_resource_edge(o,r,o,?)).
:- rdf_meta(rdf_resource_incoming_edge(o,r,o,?)).
:- rdf_meta(rdf_resource_outgoing_edge(o,r,o,?)).
:- rdf_meta(rdf_term_edge(o,r,o,?)).
:- rdf_meta(rdf_term_incoming_edge(o,r,o,?)).
:- rdf_meta(rdf_term_outgoing_edge(o,r,o,?)).

:- multifile(error:has_type/2).
error:has_type(rdf_term, Term):-
  (   rdf_is_bnode(Term)
  ;   rdf_is_literal(Term)
  ;   rdf_is_resource(Term)
  ).



%! rdf_ground_triple
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.

rdf_ground_triple(S, P, O, G):-
  rdf(S, P, O, G),
  rdf_is_ground_triple(rdf(S,P,O)).



%! rdf_id(+Term:rdf_term, +EquivTerm:rdf_term) is semidet.
%! rdf_id(+Term:rdf_term, -EquivTerm:rdf_term) is multi.
%! rdf_id(-Term:rdf_term, +EquivTerm:rdf_term) is multi.

rdf_id(T1, T2):-
  rdf_reachable(T1, owl:sameAs, T2).



%! rdf_is_ground_triple(+Triple:compound) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.

rdf_is_ground_triple(rdf(S,P,O)):-
  maplist(rdf_ground_term, [S,O])



%! rdf_resource_edge(
%!   +Term:rdf_term,
%!   -Predicate:iri,
%!   -OtherTerm:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming and outgoing edges for the resource denoted by
%  the given RDF term.

rdf_resource_edge(From, P, To, G):-
  rdf_resource_outgoing_edge(From, P, To, G).
rdf_resource_edge(To, P, From, G):-
  rdf_resource_incoming_edge(To, P, From, G).



%! rdf_resource_incoming_edge(
%!   +To:rdf_term,
%!   -Predicate:iri,
%!   -From:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming edges for the resource denoted by the given RDF term.

rdf_resource_incoming_edge(To, P, From, G):-
  rdf_id(To, To0),
  rdf(From0, P, To0, G),
  rdf_id(From, From0).



%! rdf_resource_outgoing_edge(
%!   +From:rdf_term,
%!   -Predicate:iri,
%!   -To:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns outgoing edges for the resource denoted by the given RDF term.

rdf_resource_outgoing_edge(From, P, To, G):-
  rdf_id(From, From0),
  rdf(From0, P, To0, G),
  rdf_id(To, To0).



%! rdf_term_edge(
%!   +Term:rdf_term,
%!   -Predicate:iri,
%!   -OtherTerm:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming and outgoing edges for the given RDF term.

rdf_term_edge(S, P, O, G):-
  rdf_term_outgoing_edge(S, P, O, G).
rdf_term_edge(O, P, S, G):-
  rdf_term_incoming_edge(O, P, S, G).



%! rdf_term_incoming_edge(
%!   +Object:rdf_term,
%!   -Predicate:iri,
%!   -Subject:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming edges for the given RDF term.

rdf_term_incoming_edge(O, P, S, G):-
  rdf(S, P, O, G).



%! rdf_term_outgoing_edge(
%!   +Subject:or([bnode,iri]),
%!   -Predicate:iri,
%!   -Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns outgoing edges for the given RDF term.

rdf_term_outgoing_edge(S, P, O, G):-
  rdf(S, P, O, G).



%! rdf_triples(+Graph:atom, -Triples:ordset(compound)) is det.

rdf_triples(Graph, Triples):-
  aggregate_all(
    set(rdf(S,P,O),
    rdf(S, P, O, Graph),
    Triples
  ).
