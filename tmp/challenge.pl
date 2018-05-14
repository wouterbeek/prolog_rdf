:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(service/ll_api)).
:- use_module(library(service/lotus_api)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

/** <module> Challenge

“Florence May Harding studied at a school in Sydney, and with Douglas Robert Dundas , but in effect had no formal training in either botany or art.”

| Recognized Entity     | generated URI            | Type             | SameAs                       |
|:----------------------|:-------------------------|:-----------------|:-----------------------------|
| Florence May Harding  | oke:Florence_May_Harding | dul:Person       | dbpedia:Florence_May_Harding |
| school                | oke:School               | dul:Organization |                              |
| Sydney                | oke:Sydney               | dul:Place        | dbpedia:Sydney               |
| Douglas Robert Dundas | oke:Douglas_Robert_Dundas| dul:Person       |                              |

@author Wouter Beek
@author Filip Ilievski
@version 2016/01
*/

:- debug(challenge).
%:- debug(ldf(_)).

go :-
  go_instance('Florence May Harding'),
  go_class(school),
  go_instance('Sydney'),
  go_instance('Douglas Robert Dundas'),
  true.

go_instance(String) :-
  lotus_instance(String, I),
  class_one(I, C),
  same_as(I, Js),
  rdf_print_triple(I, rdf:type, C),
  dcg_with_output_to(user_output, set(Js)).

go_class(String) :-
  aggregate_all(set(S), lotus(String, S, [rank(lengthnorm)]), Ss),
  maplist(term_to_number_of_docs, Ss, Ns),
  pairs_keys_values(Pairs, Ns, Ss),
  sort(1, @>=, Pairs, SortedPairs),
  member(_-C, SortedPairs),
  docs(_, rdf:type, C, Docs),
  maplist(class_type_goal0(C), Docs, Goals),
  (concurrent(10, Goals, []) -> fail ; !),
  writeln(C).
class_type_goal0(C, Doc, \+ ldf(_, P, C, Doc)) :-
  rdf_equal(rdf:type, P).

lotus_instance(String, One) :-
  aggregate_all(set(S), lotus(String, S), Ss),
  maplist(term_to_number_of_docs, Ss, Ns),
  pairs_keys_values(Pairs, Ns, Ss),
  sort(1, @>=, Pairs, [_-One|_]).

class_one(I, C) :-
  findall(C, ldf(I, rdf:type, C), Cs),
  sort(0, @=<, Cs, SortedCs),
  county_pairs(SortedCs, Pairs),
  desc_pairs_values(Pairs, [C|_]).

pair_value_length(Key-Val, Key-N) :-
  length(Val, N).

county_pairs([], []) :- !.
county_pairs([H|T1], [N-H|T2]) :-
  skip_same(H, 1, N, T1, T3),
  county_pairs(T3, T2).

skip_same(H, N1, N, [H|T1], T2) :- !,
  N2 is N1 + 1,
  skip_same(H, N2, N, T1, T2).
skip_same(_, N, N, L, L).

same_as(I, Js) :-
  findall(J, distinct(J, same_as0(I, J)), Js).

same_as0(I, J) :- ldf(I, owl:sameAs, J).
same_as0(I, J) :- ldf(J, owl:sameAs, I).
same_as0(I, I).
