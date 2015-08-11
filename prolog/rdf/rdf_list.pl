:- module(
  rdf_list,
  [
    rdf_assert_list/2, % +PrologList, ?RdfList
    rdf_assert_list/3, % +PrologList:list
                       % ?RdfList:or([bnode,iri])
                       % ?Graph:atom
    rdf_is_list/1, % @Term
    rdf_list/2, % +RdfList, ?PrologList
    rdf_list/3, % ?RdfList:or([bnode,iri])
                % +PrologList:list
                % ?Graph:atom
    rdf_list_raw/2, % +PrologList, ?RdfList
    rdf_list_raw/3, % +PrologList:list
                    % ?RdfList:or([bnode,iri])
                    % ?Graph:atom
    rdf_list_first/2, % ?List, ?First
    rdf_list_first/3, % ?List:or([bnode,iri])
                      % ?First:rdf_term
                      % ?Graph:atom
    rdf_list_first_raw/2, % ?List, ?First
    rdf_list_first_raw/3, % ?List:or([bnode,iri])
                          % ?First:rdf_term
                          % ?Graph:atom
    rdf_list_member/2, % ?Element, ?List
    rdf_list_member/3, % ?Element:rdf_term
                       % ?List:or([bnode,iri])
                       % ?Graph:atom
    rdf_list_member_raw/2, % ?Element, ?List
    rdf_list_member_raw/3 % ?Element:rdf_term
                          % ?List:or([bnode,iri])
                          % ?Graph:atom
  ]
).

/** <module> RDF list

Support for reading/writing RDF lists.

---

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(apply)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(typecheck)).

:- rdf_meta(rdf_assert_list(+,r)).
:- rdf_meta(rdf_assert_list(+,r,?)).
:- rdf_meta(rdf_is_list(r)).
:- rdf_meta(rdf_list(r,?)).
:- rdf_meta(rdf_list(r,?,?)).
:- rdf_meta(rdf_list_raw(r,t)).
:- rdf_meta(rdf_list_raw(r,t,?)).
:- rdf_meta(rdf_list_first(r,o)).
:- rdf_meta(rdf_list_first(r,o,?)).
:- rdf_meta(rdf_list_first_raw(r,o)).
:- rdf_meta(rdf_list_first_raw(r,o,?)).
:- rdf_meta(rdf_list_member(r,o)).
:- rdf_meta(rdf_list_member(r,o,?)).
:- rdf_meta(rdf_list_member_raw(r,o)).
:- rdf_meta(rdf_list_member_raw(r,o,?)).





%! rdf_assert_list(+PrologList:list, ?RdfList:or([bnode,iri])) is det.

rdf_assert_list(L1, L2):-
  rdf_assert_list(L1, L2, _).

%! rdf_assert_list(
%!   +PrologList:list,
%!   ?RdfList:or([bnode,iri]),
%!   ?Graph:atom
%! ) is det.
% Asserts the given, possibly nested, list into RDF.

rdf_assert_list(L1, L2, G):-
  rdf_transaction(rdf_assert_list0(L1, L2, G)).

rdf_assert_list0([], rdf:nil, _):- !.
rdf_assert_list0(L1, L2, G):-
  add_list_instance0(L2, G),
  rdf_assert_list_items0(L1, L2, G).

rdf_assert_list_items0([], rdf:nil, _).
rdf_assert_list_items0([H1|T1], L2, G):-
  % rdf:first
  (   % Nested list.
      is_list(H1)
  ->  rdf_assert_list0(H1, H2, G)
  ;   % Non-nested list.
      H2 = H1
  ),
  (   (is_uri(H2) ; rdf_is_bnode(H2))
  ->  rdf_assert2(L2, rdf:first, H2, G)
  ;   rdf_assert_literal0(L2, rdf:first, H2, G)
  ),

  % rdf:rest
  (   T1 == []
  ->  rdf_global_id(rdf:nil, T2)
  ;   add_list_instance0(T2, G),
      rdf_assert_list_items0(T1, T2, G)
  ),
  rdf_assert2(L2, rdf:rest, T2, G).

add_list_instance0(L, G):-
  (var(L) -> rdf_bnode(L) ; true),
  rdf_assert_instance(L, rdf:'List', G).



%! rdf_is_list(@Term) is semidet.

rdf_is_list(L):-
  rdf_equal(rdf:nil, L), !.
rdf_is_list(L):-
  rdfs_individual_of(L, rdf:'List'), !.
rdf_is_list(L):-
  rdf_has(L, rdf:first, _), !.
rdf_is_list(L):-
  rdf_has(L, rdf:rest, _), !.



%! rdf_list(+RdfList:or([bnode,iri]), +PrologList:list) is semidet.
%! rdf_list(+RdfList:or([bnode,iri]), -PrologList:list) is det.
% @see rdf_list/3

rdf_list(L1, L2):-
  rdf_list(L1, L2, _).

%! rdf_list(
%!   +RdfList:or([bnode,iri]),
%!   ?PrologList:list,
%!   ?Graph:atom
%! ) is semidet.

rdf_list(L1, L2, G):-
  rdf_list_raw(L1, L0, G),
  maplist(rdf_interpreted_term, L0, L2).


%! rdf_list_raw(+RdfList:or([bnode,iri]), +PrologList:list) is semidet.
%! rdf_list_raw(+RdfList:or([bnode,iri]), -PrologList:list) is det.
% @see rdf_list_raw/3

rdf_list_raw(L1, L2):-
  rdf_list_raw(L1, L2, _).

%! rdf_list_raw(
%!   +RdfList:or([bnode,iri]),
%!   ?PrologList:list,
%!   ?Graph:atom
%! ) is semidet.

rdf_list_raw(rdf:nil, [], _):- !.
rdf_list_raw(L1, [H2|T2], G):-
  % rdf:first
  rdf2(L1, rdf:first, H1, G),
  (   % Nested list
      rdf_is_list(H1)
  ->  rdf_list_raw(H1, H2, G)
  ;   % Non-nested list.
      H2 = H1
  ),
  % rdf:rest
  rdf2(L1, rdf:rest, T1, G),
  rdf_list_raw(T1, T2, G).



%! rdf_list_first(?List:or([bnode,iri]), ?First:rdf_term) is nondet.

rdf_list_first(L, X):-
  rdf_list_first(L, X, _).

%! rdf_list_first(
%!   ?List:or([bnode,iri]),
%!   ?First:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Relates RDF lists to their first element.

rdf_list_first(L, X, G):-
  rdf_list_first_raw(L, X0, G),
  rdf_interpreted_term(X0, X).


%! rdf_list_first_raw(?List:or([bnode,iri]), ?First:rdf_term) is nondet.

rdf_list_first_raw(L, X):-
  rdf_list_first_raw(L, X, _).

%! rdf_list_first_raw(
%!   ?List:or([bnode,iri]),
%!   ?First:rdf_term,
%!   ?Graph:atom
%! ) is nondet.
% Relates RDF lists to their first element.

rdf_list_first_raw(L, X, G):-
  rdf(L, rdf:first, X, G).



%! rdf_list_member(?Member:rdf_term, ?List:or([bnode,iri])) is nondet.

rdf_list_member(X, L):-
  rdf_list_member(X, L, _).

%! rdf_list_member(
%!   ?Member:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds if Member occurs in List.

rdf_list_member(X, L, G):-
  rdf_list_member_raw(X0, L, G),
  rdf_interpreted_term(X0, X).


%! rdf_list_member_raw(?Member:rdf_term, ?List:or([bnode,iri])) is nondet.

rdf_list_member_raw(X, L):-
  rdf_list_member_raw(X, L, _).

%! rdf_list_member_raw(
%!   ?Member:rdf_term,
%!   ?List:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.
% Succeeds if Member occurs in List.

rdf_list_member_raw(X, L, G):-
  rdf_list_first_raw(L, X, G).
rdf_list_member_raw(X, L, G):-
  rdf(L, rdf:rest, L0, G),
  rdf_list_member_raw(X, L0, G).
