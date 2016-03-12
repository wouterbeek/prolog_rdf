:- module(
  rdf_triples,
  [
    rdf_ground_triple/4, % ?Subject:iri
                         % ?Predicate:iri
                         % ?Object:or([iri,literal])
                         % ?Graph:atom
    rdf_is_nonground_triple/1, % @Triple
    rdf_is_triple/1, % @Triple
    rdf_nonground_triple/4, % ?Subject:or([bnode,iri])
                            % ?Predicate:iri
                            % ?Object:rdf_term
                            % ?Graph:atom
    rdf_triples/3, % +Resource:iri,
                   % -Triples:ordset(compound)
                   % ?Graph:atom
    rdf_triples_to_edges/2 % +Triples:list(compound)
                           % -Edges:ordset(pair(rdf_term))
  ]
).

/** <module> RDF triples

Support for RDF triple compound terms.

@author Wouter Beek
@version 2014/11, 2015/03, 2015/05, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_ext)).

:- rdf_meta
   rdf_ground_triple(r,r,o,?),
   rdf_is_nonground_triple(t),
   rdf_is_triple(t),
   rdf_nonground_triple(r,r,o,?),
   rdf_triples(r,-,?).





%! rdf_ground_triple(
%!   ?Subject:iri,
%!   ?Predicate:iri,
%!   ?Object:or([iri,literal]),
%!   ?Graph:atom
%! ) is nondet.

rdf_ground_triple(S, P, O, G) :-
  rdf(S, P, O, G),
  rdf_is_ground_triple(rdf(S,P,O)).



%! rdf_is_nonground_triple(@Triple) is semidet.
% Succeeds if the given triple is non-ground, i.e., contains a blank node.

rdf_is_nonground_triple(rdf(S,_,_)) :-
  rdf_is_bnode(S), !.
rdf_is_nonground_triple(rdf(_,_,O)) :-
  rdf_is_bnode(O).



%! rdf_is_triple(@Triple) is semidet.

rdf_is_triple(rdf(S,P,O)) :-
  rdf_is_subject(S),
  rdf_is_predicate(P),
  rdf_is_object(O).



%! rdf_nonground_triple(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is nondet.

rdf_nonground_triple(S, P, O, G) :-
  rdf(S, P, O, G),
  rdf_is_nonground_triple(rdf(S,P,O)).



%! rdf_triples(+Resource:iri, -Triples:ordset(compound), ?Graph:atom) is det.

rdf_triples(Resource, Triples, Graph) :-
  aggregate_all(
    set(rdf(Resource,P,O)),
    rdf_term_outgoing_edge(Resource, P, O, Graph),
    Triples
  ).



%! rdf_triples_to_edges(
%!   +Triples:list(rdf_term),
%!   -Edges:ordset(compound)
%! ) is det.

rdf_triples_to_edges(Ts, Es) :-
  aggregate_all(
    set(FromV-ToV),
    member(rdf(FromV,_,ToV), Ts),
    Es
  ).
