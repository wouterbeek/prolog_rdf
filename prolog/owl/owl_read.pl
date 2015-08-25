:- module(
  owl_read,
  [
    owl_call/2, % :Goal_1
                % +Term:rdf_term
    owl_call/3, % :Goal_2
                % +Term:rdf_term
                % -Arg2
    owl_id/2, % ?Term1:iri
              % ?Term2:iri
    owl_id/5 % ?Subject:rdf_term
             % ?Predicate:iri
             % ?Object:rdf_term
             % ?Graph:atom
             % -Triple:compound
  ]
).

/** <module> OWL read

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).

:- rdf_set_predicate(owl:sameAs, symmetric(true)).
:- rdf_set_predicate(owl:sameAs, transitive(true)).

:- meta_predicate(owl_call(1,+)).
:- meta_predicate(owl_call(2,+,?)).

:- rdf_meta(owl_call(:,r)).
:- rdf_meta(owl_call(:,r,?)).
:- rdf_meta(owl_id(r,r)).
:- rdf_meta(owl_id(o,r,o,?,-)).





%! owl_call(:Goal_1, +Term:rdf_term) .

owl_call(Goal_1, T0):-
  owl_id(T0, T),
  call(Goal_1, T).


%! owl_call(:Goal_2, +Term:rdf_term, -Arg2) .

owl_call(Goal_2, T0, Arg2):-
  owl_id(T0, T),
  call(Goal_2, T, Arg2).



%! owl_id(+Term1:rdf_term, +Term2:rdf_term) is semidet.
%! owl_id(+Term1:rdf_term, -Term2:rdf_term) is multi.
%! owl_id(-Term1:rdf_term, +Term2:rdf_term) is multi.

owl_id(X, Y):-
  rdf_reachable(X, owl:sameAs, Y).



%! owl_id(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom,
%!   -Triple:compound
%! ) is nondet.

owl_id(S1, P1, O1, G, T):-
  distinct(T, (
    (var(S1) -> true ; owl_id(S1, S2)),
    (var(P1) -> true ; owl_id(P1, P2)),
    (var(O1) -> true ; owl_id(O1, O2)),
    rdf2(S2, P2, O2, G),
    owl_id(S2, S3),
    owl_id(P2, P3),
    owl_id(O2, O3),
    T = rdf(S3, P3, O3)
  )).
