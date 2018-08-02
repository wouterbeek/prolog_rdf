:- module(
  rdf_api,
  [
    assert_triple/4,     % ?Backend, +S, +P, +O
    list_member/3,       % ?Backend, ?X, ?L
    triple/4,            % ?Backend, ?S, ?P, ?O
    triple_count/5,      % ?Backend, ?S, ?P, ?O, -N
    triple_list_member/4 % ?Backend, ?S, ?P, ?X
  ]
).

/** <module> RDF API

Backend-independent RDF API.

@author Wouter Beek
@version 2018
*/

:- use_module(library(semweb/rdf_prefix)).

:- multifile
    rdf_api:assert_triple_/4,
    rdf_api:triple_/4,
    rdf_api:triple_count_/5.

:- rdf_meta
   assert_triple(t, r, r, o),
   list_member(t, o, r),
   triple(t, r, r, o),
   triple_count(t, r, r, o, -),
   triple_list_member(t, r, r, o).





%! assert_triple(+Backend, +S:rdf_nonliteral, +P:iri, +O:rdf_term) is nondet.

assert_triple(B, S, P, O) :-
  rdf_api:assert_triple_(B, S, P, O).

rdf_api:assert_triple_(dummy, _, _, _) :-
  fail.



%! list_member(?Backend, ?X:rdf_nonliteral, ?L:rdf_list) is nondet.

list_member(B, X, L) :-
  triple(B, L, rdf:first, X).
list_member(B, X, L) :-
  triple(B, L, rdf:rest, T),
  list_member(B, X, T).



%! triple(?Backend, ?S:rdf_nonliteral, ?P:iri, ?O:rdf_term) is nondet.

triple(B, S, P, O) :-
  rdf_api:triple_(B, S, P, O).

rdf_api:triple_(dummy, _, _, _) :-
  fail.



%! triple_count(?Backend, ?S:rdf_nonliteral, ?P:iri, ?O:rdf_term, -N:nonneg) is nondet.

triple_count(B, S, P, O, N) :-
  rdf_api:triple_count_(B, S, P, O, N).

rdf_api:triple_count_(dummy, _, _, _, 0) :-
  fail.



%! triple_list_member(?Backend, ?S:rdf_nonliteral, ?P:iri, ?X:rdf_term) is nondet.

triple_list_member(B, S, P, X) :-
  ground(X), !,
  list_member(B, X, L),
  triple(B, S, P, L).
triple_list_member(B, S, P, X) :-
  triple(B, S, P, L),
  list_member(B, X, L).
