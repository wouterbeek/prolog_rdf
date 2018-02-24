:- module(lodir, [query/1]).

:- use_module(library(apply)).
:- use_module(library(default)).
:- use_module(library(http/http_io)).
:- use_module(library(http/json)).
:- use_module(library(lists)).
:- use_module(library(uri)).

query(Q):-
  query(Q, A0),
  sort(2, @>=, A0, A),
  length(A1, 10),
  (append(A1, _, A) -> A2 = A1 ; A2 = []),
  maplist(writeln, A2).

query(and(Q1,not(Q2)), A):- !,
  maplist(query, [Q1,Q2], [A1,A2]),
  answer_filter(A1, A2, A).
query(and(Q1,Q2), A):- !,
  maplist(query, [Q1,Q2], [A1,A2]),
  answer_intersection(A1, A2, A).
query(Q, A):-
  query(Q, 100, A).

%! query(+Query:atom, ?NumberOfResults:nonneg, -Results:list(compound)) is det.

query(Query, NResults, L):-
  default_value(NResults, 10),
  uri_query_components(Search, [pattern=Query,size=NResults]),
  uri_components(Uri, uri_components(http,'textindex.fii800.d2s.labs.vu.nl','/phrase',Search,_)),
  http_get(Uri, json_read_dict0(D0)),
  D = D0.hits.hits,
  maplist(answer_term, D, L0),
  sort(L0, L).

json_read_dict0(Result, _, Read):-
  json_read_dict(Read, Result).

is_label(D):-
  D.'_source'.subject == 'http://www.w3.org/2000/01/rdf-schema#label'.

answer_term(D0, answer(D0.'_source'.subject,D0.'_score')).

answer_filter([], _, []):- !.
answer_filter([answer(S,_)|T1], L2, L3):-
  memberchk(answer(S,_), L2), !,
  %format(user_output, 'Removing ~a\n', S),
  answer_filter(T1, L2, L3).
answer_filter([answer(S,N1)|T1], L2, [answer(S,N1)|T3]):-
  answer_filter(T1, L2, T3).

answer_intersection([], L, L):- !.
answer_intersection([answer(S,N1)|T1], L2a, [answer(S,N3)|T3]):-
  select(answer(S,N2), L2a, L2b), !,
  N3 is N1 + N2,
  answer_intersection(T1, L2b, T3).
answer_intersection([answer(S,N1)|T1], L2, [answer(S,N3)|T3]):-
  N3 is N1,
  answer_intersection(T1, L2, T3).
