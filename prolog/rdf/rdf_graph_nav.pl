:- module(
  rdf_graph_nav,
  [
    rdf_resource_edge/4, % +From:rdf_term
                         % ?Predicate:iri
                         % -To:rdf_term
                         % ?Graph:atom
    rdf_resource_incoming_edge/4, % +From:rdf_term
                                  % ?Predicate:iri
                                  % -To:rdf_term
                                  % ?Graph:atom
    rdf_resource_outgoing_edge/4, % +From:rdf_term
                                  % ?Predicate:iri
                                  % -To:rdf_term
                                  % ?Graph:atom
    rdf_term_edge/4, % ?From:rdf_term
                     % ?Predicate:iri
                     % ?To:rdf_term
                     % ?Graph:atom
    rdf_term_incoming_edge/4, % ?From:rdf_term
                              % ?Predicate:iri
                              % ?To:rdf_term
                              % ?Graph:atom
    rdf_term_outgoing_edge/4 % ?From:rdf_term
                             % ?Predicate:iri
                             % ?To:rdf_term
                             % ?Graph:atom
  ]
).

/** <module> RDF graph navigation

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_resource_edge(t,r,t,?)).
:- rdf_meta(rdf_resource_incoming_edge(t,r,r,?)).
:- rdf_meta(rdf_resource_outgoing_edge(r,r,t,?)).
:- rdf_meta(rdf_term_edge(t,r,t,?)).
:- rdf_meta(rdf_term_incoming_edge(t,r,r,?)).
:- rdf_meta(rdf_term_outgoing_edge(r,r,t,?)).





%! rdf_resource_edge(
%!   +Term:rdf_term,
%!   ?Predicate:iri,
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
%!   ?Predicate:iri,
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
%!   ?Predicate:iri,
%!   -To:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns outgoing edges for the resource denoted by the given RDF term.

rdf_resource_outgoing_edge(From, P, To, G):-
  rdf_id(From, From0),
  rdf(From0, P, To0, G),
  rdf_id(To, To0).



%! rdf_term_edge(
%!   ?Term:rdf_term,
%!   ?Predicate:iri,
%!   ?OtherTerm:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming and outgoing edges for the given RDF term.

rdf_term_edge(S, P, O, G):-
  rdf_term_outgoing_edge(S, P, O, G).
rdf_term_edge(O, P, S, G):-
  rdf_term_incoming_edge(O, P, S, G).



%! rdf_term_incoming_edge(
%!   ?Object:rdf_term,
%!   ?Predicate:iri,
%!   ?Subject:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Returns incoming edges for the given RDF term.

rdf_term_incoming_edge(O, P, S, G):-
  rdf(S, P, O, G).



%! rdf_term_outgoing_edge(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Returns outgoing edges for the given RDF term.

rdf_term_outgoing_edge(S, P, O, G):-
  rdf(S, P, O, G).
