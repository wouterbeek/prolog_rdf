:- module(
  rdf_graph_nav,
  [
    rdf_edge/4, % +From:rdf_term
                % ?Predicate:iri
                % -To:rdf_term
                % ?Graph:rdf_graph
    rdf_ego/3, % +Term:rdf_term
               % +Depth:nonneg
               % -Triples:list(rdf_triple)
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
@version 2015/07-2015/08, 2015/12, 2016/02
*/

:- use_module(library(aggregate)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_statement)).
:- use_module(library(rdf11/rdf11)).

:- rdf_meta
   rdf_edge(o,r,o,?),
   rdf_ego(o,+,-),
   rdf_incoming_edge(o,r,o,?),
   rdf_outgoing_edge(o,r,o,?).





%! rdf_edge(
%!   +Term:rdf_term,
%!   ?Predicate:iri,
%!   -OtherTerm:rdf_term,
%!   ?Graph:rdf_graph
%! ) is nondet.
% Returns incoming and outgoing edges for the resource denoted by
% the given RDF term.

rdf_edge(From, P, To, G) :-
  rdf_outgoing_edge(From, P, To, G).
rdf_edge(To, P, From, G) :-
  rdf_incoming_edge(To, P, From, G).



%! rdf_ego(+Term:rdf_term, +Depth:nonneg, -Triples:ordset(rdf_triple)) is det.

rdf_ego(T, N, Ts) :-
  rdf_ego([T], N, [], Ts).

rdf_ego([], _, Ts, Ts) :- !.
rdf_ego([S|Ss1], N1, Ts1, Ts) :-
  aggregate_all(set(rdf(S,P,O)), rdf(S, P, O), Ts0),
  rdf_triples_subjects(Ts0, Ss0),
  ord_union(Ts1, Ts0, Ts2),
  ord_union(Ss1, Ss0, Ss2),
  N2 is N1 + 1,
  rdf_ego(Ss2, N2, Ts2, Ts).



%! rdf_incoming_edge(
%!   +To:rdf_term,
%!   ?Predicate:iri,
%!   -From:rdf_term,
%!   ?Graph:rdf_graph
%! ) is nondet.
% Returns incoming edges for the resource denoted by the given RDF term.

rdf_incoming_edge(To, P, From, G) :-
  rdf(From, P, To, G).



%! rdf_outgoing_edge(
%!   +From:rdf_term,
%!   ?Predicate:iri,
%!   -To:rdf_term,
%!   ?Graph:rdf_graph
%! ) is nondet.
% Returns outgoing edges for the resource denoted by the given RDF term.

rdf_outgoing_edge(From, P, To, G) :-
  rdf(From, P, To, G).
