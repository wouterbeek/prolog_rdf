:- module(
  hdt_ext,
  [
    hdt/4,        % +File, ?S, ?P, ?O
    hdt0/4,       % +Hdt,  ?S, ?P, ?O
    hdt_goal/2,   % +File, :Goal_1
    hdt_last/3,   % +File, ?X, ?L
    hdt_last0/3,  % +Hdt,  ?X, ?L
    hdt_member/3, % +File, ?X, ?L
    hdt_member0/3 % +Hdt,  ?X, ?L
  ]
).
:- reexport(library(hdt)).

/** <module> HDT extensions

@author Wouter Beek
@version 2016/04
*/

:- use_module(library(hdt)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- rdf_register_prefix(deref, 'http://lodlaundromat.org/deref/').

:- rdf_meta
   hdt(+, r, r, o),
   hdt0(+, r, r, o),
   hdt_last(+, r, r),
   hdt_last0(+, r, r),
   hdt_member(+, r, r),
   hdt_member0(+, r, r).

:- meta_predicate
    hdt_goal(+, 1).





hdt(File, S, P, O) :-
  hdt_goal(File, {S,P,O}/[Hdt]>>hdt0(Hdt, S, P, O)).


hdt0(Hdt, S, P, O) :-
  hdt_search(Hdt, S, P, O).



hdt_goal(File, Goal_1) :-
  setup_call_cleanup(
    hdt_open(Hdt, File),
    call(Goal_1, Hdt),
    hdt_close(Hdt)
  ).



hdt_last(File, L, X) :-
  hdt_goal(File, {L,X}/[Hdt]>>hdt_last0(Hdt, L, X)).


hdt_last0(Hdt, L, X) :-
  rdf_is_subject(L), !,
  hdt0(Hdt, L, rdf:rest, T),
  (   rdf_equal(T, rdf:nil)
  ->  hdt0(Hdt, L, rdf:first, X)
  ;   hdt_last0(Hdt, T, X)
  ).



hdt_member(File, X, L) :-
  hdt_goal(File, {X,L}/[Hdt]>>hdt_member0(Hdt, X, L)).


hdt_member0(Hdt, X, L) :-
  ground(X), !,
  (   hdt_member2(Hdt, X, L)
  ->  true
  ).
hdt_member0(Hdt, X, L) :-
  hdt_member2(Hdt, X, L).


hdt_member2(Hdt, X, L) :-
  hdt0(Hdt, L, rdf:first, X).
hdt_member2(Hdt, X, L) :-
  hdt0(Hdt, L, rdf:rest, L0),
  hdt_member2(Hdt, X, L0).
