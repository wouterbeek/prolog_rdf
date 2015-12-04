:- module(
  rdf_graph_nav,
  [
    rdf_edge/4, % +From:rdf_term
                % ?Predicate:iri
                % -To:rdf_term
                % ?Graph:rdf_graph
    rdf_incoming_edge/4, % +From:rdf_term
                         % ?Predicate:iri
                         % -To:rdf_term
                         % ?Graph:rdf_graph
    rdf_outgoing_edge/4 % +From:rdf_term
                        % ?Predicate:iri
                        % -To:rdf_term
                        % ?Graph:rdf_graph
  ]
).

/** <module> RDF graph navigation

@author Wouter Beek
@version 2015/07-2015/08, 2015/12
*/

:- use_module(library(rdf/rdf_read)).

:- rdf_meta(rdf_edge(o,r,o,?)).
:- rdf_meta(rdf_incoming_edge(o,r,o,?)).
:- rdf_meta(rdf_outgoing_edge(o,r,o,?)).





%! rdf_edge(
%!   +Term:rdf_term,
%!   ?Predicate:iri,
%!   -OtherTerm:rdf_term,
%!   ?Graph:rdf_graph
%! ) is nondet.
% Returns incoming and outgoing edges for the resource denoted by
% the given RDF term.

rdf_edge(From, P, To, G):-
  rdf_outgoing_edge(From, P, To, G).
rdf_edge(To, P, From, G):-
  rdf_incoming_edge(To, P, From, G).



%! rdf_incoming_edge(
%!   +To:rdf_term,
%!   ?Predicate:iri,
%!   -From:rdf_term,
%!   ?Graph:rdf_graph
%! ) is nondet.
% Returns incoming edges for the resource denoted by the given RDF term.

rdf_incoming_edge(To, P, From, G):-
  rdf(From, P, To, G).



%! rdf_outgoing_edge(
%!   +From:rdf_term,
%!   ?Predicate:iri,
%!   -To:rdf_term,
%!   ?Graph:rdf_graph
%! ) is nondet.
% Returns outgoing edges for the resource denoted by the given RDF term.

rdf_outgoing_edge(From, P, To, G):-
  rdf(From, P, To, G).
