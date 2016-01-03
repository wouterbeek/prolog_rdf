:- module(
  rdf_list,
  [
    rdf_assert_list/2,	% +PrologList, -RdfList
    rdf_assert_list/3,	% +PrologList, -RdfList, +G
    rdf_last/2,		% ?RdfList, ?Last
    rdf_list/1,		% ?RdfList
    rdf_list/2,		% +RdfList, -PrologList
    rdf_list_length/2,	% ?RdfList, ?Length
    rdf_list_member/2,	% ?Member, +RdfList
    rdf_nextto/3,	% ?X, ?Y, ?RdfList
    rdf_nth0/3,		% ?Index, ?RdfList, ?Element
    rdf_retract_lists/1	% +RdfList
  ]
).

/** <module> RDF lists

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(closure)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_database)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdf11/rdf11), [rdf_literal_value/2]).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).

:- rdf_meta
	rdf_assert_list(t, r),
	rdf_assert_list(t, r, r),
	rdf_last(r, o),
	rdf_list(r),
	rdf_list(r, -),
	rdf_list_length(r, -),
	rdf_list_member(o, r),
	rdf_nextto(o, o, r),
	rdf_nth0(?, r, o),
	rdf_retract_lists(r).





%! rdf_assert_list(+PrologList, -RdfList) is det.
%! rdf_assert_list(+PrologList, -RdfList, +Graph) is det.
% Create an RDF list from the given Prolog List.
%
% Unexpanded IRIs in PrologList are expanded according to
% the current prefix declarations.
% The Prolog Boolean values, floats, integers and strings
% are converted to RDF literals as per pre_ground_object/2.

rdf_assert_list(L1, L2) :-
  rdf_assert_list(L1, L2, _).

rdf_assert_list(L1, L2, G) :-
  rdf_transaction(rdf_assert_list0(L1, L2, G)).

rdf_assert_list0([], Nil, _) :-
  rdf_equal(rdf:nil, Nil).
rdf_assert_list0([H1|T1], L2, G) :-
  rdf_assert_list0(T1, T2, G),
  (var(L2) -> rdf_create_bnode(L2) ; true),
  rdf_assert(L2, rdf:type, rdf:'List', G),
  (is_list(H1) -> rdf_assert_list0(H1, H2, G) ; H2 = H1),
  rdf_assert(L2, rdf:first, H2, G),
  rdf_assert(L2, rdf:rest, T2, G).


%! rdf_last(?RdfList, ?Last) is semidet.

rdf_last(L, Last) :-
  rdf_has(L, rdf:rest, rdf:nil), !,
  rdf_has(L, rdf:first, Last).
rdf_last(L, Last) :-
  rdf_has(L, rdf:rest, T),
  rdf_last(T, Last).



%! rdf_list_nth0(?Index:nonneg, ?RdfList, ?X) is nondet.

rdf_nth0(I, L, X) :-
  rdf_nth0(0, I, L, X).

rdf_nth0(I, I, L, X) :-
  rdf_has(L, rdf:first, X).
rdf_nth0(I1, I3, L, X) :-
  rdf_has(L, rdf:rest, T),
  rdf_nth0(I1, I2, T, X),
  I3 is I2 + 1.



%! rdf_list(+Term) is semidet.
%! rdf_list(-RdfList) is semidet.
% Succeeds iff the given term denotes an RDF list.

rdf_list(L) :- (var(L) -> distinct(L, rdf_list0(L)) ; rdf_is_subject(L) -> once(rdf_list0(L))).
rdf_list0(L) :- rdf_has(L, rdf:first, _).
rdf_list0(L) :- rdf_has(L, rdf:rest, _).
rdf_list0(L) :- rdf_equal(rdf:nil, L).


%! rdf_list(+RdfList, -PrologList) is nondet.
% Reads an asserted RDF list as Prolog list, interpreting RDF literals
% as Prolog values according to pre_ground_object/2.
%
% Notice that `?- rdf_assert_list(L1, X), rdf_list(X, L2), L1 = L2.'
% will generally not succeed since literals in L1 may be read back
% as interpreted Prolog values in L2.
%
% Non-deterministically returns all lists expressed by the RdfList node.

rdf_list(L, []) :-
  rdf_equal(rdf:nil, L).
rdf_list(L1, [H2|T2]) :-
  rdf_has(L1, rdf:first, H1),
  (   rdf_list(H1)
  ->  rdf_list(H1, H2)
  ;   rdf_is_literal(H1)
  ->  rdf_literal_value(H1, H2)
  ;   H2 = H1
  ),
  rdf_has(L1, rdf:rest, T1),
  rdf_list(T1, T2).


%! rdf_list_length(+RdfList, +Length:nonneg) is semidet.
%! rdf_list_length(+RdfList, -Length:nonneg) is nondet.
%! rdf_list_length(-RdfList, -Length:nonneg) is nondet.

rdf_list_length(L, Len) :-
  rdf_list(L),
  (   rdf_equal(rdf:nil, L)
  ->  Len = 0
  ;   rdf_has(L, rdf:rest, L0),
      rdf_list_length(L0, Len0),
      Len is Len0 + 1
  ).


%! rdf_list_member(+Member, +RdfList) is semidet.
%! rdf_list_member(?Member, +RdfList) is nondet.

rdf_list_member(M, L) :-
  rdf_has(L, rdf:first, M).
rdf_list_member(M, L) :-
  rdf_has(L, rdf:rest, L0),
  rdf_list_member(M, L0).


%! rdf_nextto(?X, ?Y, ?RdfList) is nondet.

rdf_nextto(X, Y, L) :-
  closure([X,Y]>>rdf_directly_nextto(X, Y, L), X, Y).

rdf_directly_nextto(X, Y, L) :-
  rdf_has(L, rdf:first, X),
  rdf_has(L, rdf:rest, T),
  rdf_has(T, rdf:first, Y).



%! rdf_retract_lists(+RdfList) is det.

rdf_retract_lists(L) :-
  rdf_transaction(rdf_retract_lists0(L)).
rdf_retract_lists0(L) :-
  forall((rdf_has(L, rdf:first, L0), rdf_list(L0)), rdf_retract_lists(L0)),
  rdf_retractall(L, rdf:first, _),
  forall(rdf_has(L, rdf:rest, L0), rdf_retract_lists(L0)),
  rdf_retractall(L, rdf:rest, _),
  rdf_retractall(L, rdf:type, rdf:'List').
