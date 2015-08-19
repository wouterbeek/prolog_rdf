:- module(
  owl_read,
  [
    owl_call/2, % :Goal_1
                % +Term:rdf_term
    owl_call/3, % :Goal_2
                % +Term:rdf_term
                % -Arg2
    owl_id/2 % ?Term1:iri
             % ?Term2:iri
  ]
).
:- reexport(library(rdf/rdf_read)).

/** <module> OWL read

@author Wouter Beek
@version 2015/08
*/

:- meta_predicate(owl_call(1,+)).
:- meta_predicate(owl_call(2,+,?)).

:- rdf_meta(owl_call(:,r)).
:- rdf_meta(owl_call(:,r,?)).
:- rdf_meta(owl_id(r,r)).





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
