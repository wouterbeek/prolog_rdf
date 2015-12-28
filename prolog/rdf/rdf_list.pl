:- module(
  rdf_list,
  [
    rdf_assert_list/2, % +PrologList, ?RdfList
    rdf_assert_list/3, % +PrologList:list
                       % ?RdfList:rdf_term
                       % ?Graph:rdf_graph
    rdf_is_list/1, % @Term
    rdf_list/2, % +List, ?PrologList
    rdf_list/3, % ?List:rdf_term
                % +PrologList:list
                % ?Graph:rdf_graph
    rdf_list_raw/2, % +PrologList, ?RdfList
    rdf_list_raw/3, % +PrologList:list
                    % ?RdfList:rdf_term
                    % ?Graph:rdf_graph
    rdf_list_first/2, % ?List, ?First
    rdf_list_first/3, % ?List:rdf_term
                      % ?First:rdf_term
                      % ?Graph:rdf_graph
    rdf_list_first_raw/2, % ?List, ?First
    rdf_list_first_raw/3, % ?List:rdf_term
                          % ?First:rdf_term
                          % ?Graph:rdf_graph
    rdf_list_length/2, % ?List, ?Length
    rdf_list_length/3, % ?List:rdf_term
                       % ?Length:nonneg
                       % ?Graph:rdf_graph
    rdf_list_member/2, % ?Element, ?List
    rdf_list_member/3, % ?Element:rdf_term
                       % ?List:rdf_term
                       % ?Graph:rdf_graph
    rdf_list_member_raw/2, % ?Element, ?List
    rdf_list_member_raw/3, % ?Element:rdf_term
                           % ?List:rdf_term
                           % ?Graph:rdf_graph
    rdf_retractall_list/1, % +List
    rdf_retractall_list/2 % +List:rdf_term
                          % ?Graph:rdf_graph
  ]
).

/** <module> RDF list

Support for reading/writing RDF lists.

# RDF lists under entailment

For triple [1], simple entailment deduces [2].
Do we want to represent both `_:x` and  `_:y` as RDF lists?

```ntriples
[1]   _:x rdf:type rdf:List .
[2]   _:y rdf:type rdf:List .
```

Basically [1] and [2] both states that there is a list (nothing more).
Yet [1] is an RDF list, syntactically speaking,
i.e., it occurs in the subject position of a triple with predicate
`rdf:first`.

---

@author Wouter Beek
@version 2015/07-2015/10, 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_database)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(typecheck)).

:- rdf_meta(rdf_assert_list(t,r)).
:- rdf_meta(rdf_assert_list(t,r,r)).
:- rdf_meta(rdf_is_list(r)).
:- rdf_meta(rdf_list(r,?)).
:- rdf_meta(rdf_list(r,?,r)).
:- rdf_meta(rdf_list_raw(r,t)).
:- rdf_meta(rdf_list_raw(r,t,r)).
:- rdf_meta(rdf_list_first(r,o)).
:- rdf_meta(rdf_list_first(r,o,r)).
:- rdf_meta(rdf_list_first_raw(r,o)).
:- rdf_meta(rdf_list_first_raw(r,o,r)).
:- rdf_meta(rdf_list_length(r,?)).
:- rdf_meta(rdf_list_length(r,?,r)).
:- rdf_meta(rdf_list_member(r,o)).
:- rdf_meta(rdf_list_member(r,o,r)).
:- rdf_meta(rdf_list_member_raw(r,o)).
:- rdf_meta(rdf_list_member_raw(r,o,r)).
:- rdf_meta(rdf_retractall_list(r)).
:- rdf_meta(rdf_retractall_list(r,r)).





%! rdf_assert_list(+PrologList:list, ?List:rdf_term) is det.

rdf_assert_list(L1, L2):-
  rdf_assert_list(L1, L2, _).


%! rdf_assert_list(+PrologList:list, ?List:rdf_term, ?Graph:rdf_graph) is det.
% Asserts the given, possibly nested, list into RDF.

rdf_assert_list(L1, L2, G):-
  rdf_transaction(rdf_assert_list0(L1, L2, G)).

rdf_assert_list0([], rdf:nil, _):- !.
rdf_assert_list0(L1, L2, G):-
  add_list_instance0(L2, G),
  rdf_assert_list_items0(L1, L2, G).

rdf_assert_list_items0([], L, _):-
  rdf_expand_ct(rdf:nil, L), !.
rdf_assert_list_items0([H1|T1], L2, G):-
  % rdf:first
  (   % Nested list.
      is_list(H1)
  ->  rdf_assert_list0(H1, H2, G)
  ;   % Non-nested list.
      H2 = H1
  ),
  (   (is_iri(H2) ; rdf_is_bnode(H2))
  ->  rdf_assert(L2, rdf:first, H2, G)
  ;   rdf_assert_literal_pl(L2, rdf:first, H2, G)
  ),

  % rdf:rest
  (   T1 == []
  ->  rdf_expand_ct(rdf:nil, T2)
  ;   add_list_instance0(T2, G),
      rdf_assert_list_items0(T1, T2, G)
  ),
  rdf_assert(L2, rdf:rest, T2, G).

add_list_instance0(L, G):-
  (var(L) -> rdf_create_bnode(L) ; true),
  rdf_assert_instance(L, rdf:'List', G).



%! rdf_is_list(@Term) is semidet.

rdf_is_list(L):-
  rdf_expand_ct(rdf:nil, L), !.
rdf_is_list(L):-
  rdfs_instance(L, rdf:'List'), !.
rdf_is_list(L):-
  rdf(L, rdf:first, _), !.
rdf_is_list(L):-
  rdf(L, rdf:rest, _).



%! rdf_list(+List:rdf_term, +PrologList:list) is semidet.
%! rdf_list(+List:rdf_term, -PrologList:list) is det.
% Wrapper around rdf_list/3 with uninstantiated graph.

rdf_list(L1, L2):-
  rdf_list(L1, L2, _).


%! rdf_list(
%!   +List:rdf_term,
%!   ?PrologList:list,
%!   ?Graph:rdf_graph
%! ) is semidet.

rdf_list(L1, L2, G):-
  rdf_list_raw(L1, L0, G),
  maplist(rdf_interpreted_term, L0, L2).



%! rdf_list_raw(+List:rdf_term, +PrologList:list) is semidet.
%! rdf_list_raw(+List:rdf_term, -PrologList:list) is det.
% Wrapper around rdf_list_raw/3 with uninstantiated graph.

rdf_list_raw(L1, L2):-
  rdf_list_raw(L1, L2, _).


%! rdf_list_raw(+List:rdf_term, ?PrologList:list, ?Graph:rdf_graph) is semidet.

rdf_list_raw(L, [], _):-
  rdf_expand_ct(rdf:nil, L), !.
rdf_list_raw(L1, [H2|T2], G):-
  % rdf:first
  rdf(L1, rdf:first, H1, G),
  (   % Nested list
      rdf_is_list(H1)
  ->  rdf_list_raw(H1, H2, G)
  ;   % Non-nested list.
      H2 = H1
  ),
  % rdf:rest
  rdf(L1, rdf:rest, T1, G),
  rdf_list_raw(T1, T2, G).



%! rdf_list_first(?List:rdf_term, ?First:rdf_term) is nondet.
% Wrapper around rdf_list_first/3 with uninstantiated graph.

rdf_list_first(L, X):-
  rdf_list_first(L, X, _).


%! rdf_list_first(?List:rdf_term, ?First:rdf_term, ?Graph:rdf_graph) is nondet.
% Relates RDF lists to their first element.

rdf_list_first(L, X, G):-
  rdf_list_first_raw(L, X0, G),
  rdf_interpreted_term(X0, X).



%! rdf_list_first_raw(?List:rdf_term, ?First:rdf_term) is nondet.
% Wrapper around rdf_list_first_raw/3 with uninstantiated graph.

rdf_list_first_raw(L, X):-
  rdf_list_first_raw(L, X, _).


%! rdf_list_first_raw(?List:rdf_term, ?First:rdf_term, ?Graph:rdf_graph) is nondet.
% Relates RDF lists to their first element.

rdf_list_first_raw(L, X, G):-
  rdf(L, rdf:first, X, G).



%! rdf_list_length(+List:rdf_term, +Length:nonneg) is semidet.
%! rdf_list_length(+List:rdf_term, -Length:nonneg) is det.
% Wrapper around rdf_list_length/3 with uninstantiated graph.

rdf_list_length(L, N):-
  rdf_list_length(L, N, _).


%! rdf_list_length(+List:rdf_term, +Length:nonneg, ?Graph:rdf_graph) is semidet.
%! rdf_list_length(+List:rdf_term, -Length:nonneg, ?Graph:rdf_graph) is det.

rdf_list_length(L, 0, _):-
  rdf_expand_ct(rdf:nil, L), !.
rdf_list_length(L, N, G):-
  rdf(L, rdf:rest, T, G),
  rdf_list_length(T, M, G),
  succ(M, N).



%! rdf_list_member(?Member:rdf_term, ?List:rdf_term) is nondet.
% Wrapper around rdf_list_member/3 with uninstantiated graph.

rdf_list_member(X, L):-
  rdf_list_member(X, L, _).


%! rdf_list_member(?Member:rdf_term, ?List:rdf_term, ?Graph:rdf_graph) is nondet.
% Succeeds if Member occurs in List.

rdf_list_member(X, L, G):-
  rdf_list_member_raw(X0, L, G),
  rdf_interpreted_term(X0, X).



%! rdf_list_member_raw(?Member:rdf_term, ?List:rdf_term) is nondet.
% Wrapper around rdf_list_member_raw/3 with uninstantiated graph.

rdf_list_member_raw(X, L):-
  rdf_list_member_raw(X, L, _).


%! rdf_list_member_raw(
%!   ?Member:rdf_term,
%!   ?List:rdf_term,
%!   ?Graph:rdf_graph
%! ) is nondet.
% Succeeds if Member occurs in List.

rdf_list_member_raw(X, L, G):-
  rdf_list_first_raw(L, X, G).
rdf_list_member_raw(X, L, G):-
  rdf(L, rdf:rest, L0, G),
  rdf_list_member_raw(X, L0, G).



%! rdf_retractall_list(+List:rdf_term) is det.
% Wrapper around rdf_retractall_list/2 with uninstantiated graph.

rdf_retractall_list(L):-
  rdf_retractall_list(L, _).


%! rdf_retractall_list(+List:rdf_term, ?Graph:rdf_graph) is det.

rdf_retractall_list(L, _):-
  rdf_expand_ct(rdf:nil, L), !.
rdf_retractall_list(L, G):-
  % Remove the head.
  rdf(L, rdf:first, H, G),
  rdf_retractall(L, rdf:first, H, G),
  % Recurse if the head is itself a list.
  (rdf_is_list(H) -> rdf_retractall_list(H) ; true),
  
  % Remove the tail.
  rdf(L, rdf:rest, T, G),
  rdf_retractall(L, rdf:rest, T, G),
  rdf_retractall_list(T).
