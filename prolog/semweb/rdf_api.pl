:- module(
  rdf_api,
  [
    assert_instance/3,           % +Backend, +I, +C
    assert_list/3,               % +Backend, +PrologList, -RdfList
    assert_list_triple/4,        % +Backend, +S, +P, +PrologList
    assert_reification/5,        % +Backend, +S, +P, +O, -Statement
    assert_reification_triple/5, % +Backend, +S, +P, +O, -Statement
    assert_triple/4,             % +Backend, +S, +P, +O
    instance/3,                  % ?Backend, ?I, ?C
    list/3,                      % ?Backend, +RdfList, -PrologList
    list_compare/4,              % +Backend, +X, +Y, -Order
    list_last/3,                 % ?Backend, ?RdfList, ?X
    list_last_triple/4,          % ?Backend, ?S, ?P, ?X
    list_member/3,               % ?Backend, ?X, ?RdfList
    list_member_triple/4,        % ?Backend, ?S, ?P, ?X
    list_triple/4,               % ?Backend, ?S, ?P, -PrologList
    node/2,                      % ?Backend, ?Node
    reification/5,               % ?Backend, ?S, ?P, ?O, -Statement
    retractall_triples/4,        % +Backend, ?S, ?P, ?O
    strict_subclass/3,           % ?Backend, ?C, ?D
    subclass/3,                  % ?Backend, ?C, ?D
    term_string/3,               % ?Backend, +Term, -String
    triple/4,                    % ?Backend, ?S, ?P, ?O
    triple_chk/4,                % ?Backend, ?S, ?P, ?O
    triple_count/5               % ?Backend, ?S, ?P, ?O, -N
  ]
).

/** <module> RDF API

Backend-independent RDF API.

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).

:- use_module(library(closure)).
:- use_module(library(default)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).

:- maplist(rdf_register_prefix, [
     rdf,
     rdfs
   ]).

:- multifile
    rdf_api:assert_triple_/4,
    rdf_api:retractall_triples_/4,
    rdf_api:triple_/4,
    rdf_api:triple_count_/5.

:- rdf_meta
   assert_instance(t, r, r),
   assert_list(t, t, r),
   assert_list_triple(t, r, r, t),
   assert_reification(t, r, r, o, r, r),
   assert_reification(t, r, r, o, r, r),
   assert_triple(t, r, r, o),
   instance(t, r, o),
   list(t, r, -),
   list_compare(t, o, o, -),
   list_last(t, r, t),
   list_last_triple(t, r, r, o),
   list_member(t, o, r),
   list_member_triple(t, r, r, o),
   list_triple(t, r, r, -),
   node(t, o),
   reification(t, r, r, o, -),
   retractall_triples(t, r, r, o),
   strict_subclass(t, r, r),
   subclass(t, r, r),
   term_string(t, o, -),
   triple(t, r, r, o),
   triple_chk(t, r, r, o),
   triple_count(t, r, r, o, -).





%! assert_instance(+Backend, +Instance:rdf_subject, +Class:rdf_subject) is det.

assert_instance(B, I, C) :-
  assert_triple(B, I, rdf:type, C).



%! assert_list(+Backend, +PrologList:list(term), -RdfList:rdf_subject) is det.

assert_list(B, L1, L2) :-
  assert_list_(B, L1, L2).

assert_list_(_, [], rdf:nil) :- !.
assert_list_(B, [H1|T1], L2) :-
  call_default_value(L2, rdf_bnode_iri),
  assert_triple(B, L2, rdf:first, H1),
  (   T1 == []
  ->  rdf_equal(rdf:nil, T2),
      assert_triple(B, L2, rdf:rest, T2)
  ;   rdf_bnode_iri(T2),
      assert_triple(B, L2, rdf:rest, T2),
      assert_list_(B, T1, T2)
  ).



%! assert_list_triple(+Backend, +S:rdf_subject, +P:rdf_preficate, +L:list(term)) is det.

assert_list_triple(B, S, P, L1) :-
  assert_list_(B, L1, L2),
  assert_triple(B, S, P, L2).



%! assert_reification(+Backend, +S:rdf_subject, +P:rdf_predicate, +O:rdf_object, +Statement:rdf_subject) is nondet.

assert_reification(B, S, P, O, Stmt) :-
  assert_triple(B, Stmt, rdf:subject, S),
  assert_triple(B, Stmt, rdf:predicate, P),
  assert_triple(B, Stmt, rdf:object, O).



%! assert_reification_triple(+Backend, +S:rdf_subject, +P:rdf_predicate, +O:rdf_object, +Statement:rdf_subject) is det.
%
% If Statement is not given a fresh blank node is created.

assert_reification_triple(B, S, P, O, Stmt) :-
  call_default_value(Stmt, rdf_bnode_iri),
  assert_instance(B, Stmt, rdf:'Statement'),
  assert_triple(B, Stmt, rdf:subject, S),
  assert_triple(B, Stmt, rdf:predicate, P),
  assert_triple(B, Stmt, rdf:object, O),
  assert_triple(B, S, P, O).



%! assert_triple(+Backend, +S:rdf_subject, +P:rdf_predicate, +O:rdf_object) is nondet.

assert_triple(B, S, P1, O1) :-
  rdf_predicate_dwim(P1, P2),
  rdf_object_dwim(O1, O2),
  rdf_api:assert_triple_(B, S, P2, O2).

rdf_api:assert_triple_(dummy, _, _, _) :-
  fail.



%! instance(?Backend, ?Instance:rdf_term, ?Class:rdf_term) is nondet.

instance(B, I, D) :-
  ground(I), !,
  triple(B, I, rdf:type, C),
  path_closure0(subclass_(B), C, D).
instance(B, I, D) :-
  ground(D), !,
  path_closure0(subclass_(B), C, D),
  triple(B, I, rdf:type, C).
instance(_, C, D) :-
  instantiation_error(args([C,D])).



%! list(?Backend, +RdfList:rdf_subject, -PrologList:list(rdf_term)) is nondet.

list(B, L1, L2) :-
  % Ensure this is an RDF list.
  triple_chk(B, L1, rdf:rest, _),
  findall(X, list_member(B, X, L1), L2).



%! list_compare(+Backend, +X:rdf_term, +Y:rdf_term, -Order:oneof([<,0,>])) is det.
%
% When X and Y are RDF terms in the same RDF list, Order is unified
% with their respective order:
%
%   - `<' when X appears before Y in the list.
%
%   - `0' when X and Y are the same list item.
%
%   - `>' when X apears after Y in the list.

list_compare(B, X, X, 0) :-
  triple_chk(B, _, rdf:first, X), !.
list_compare(B, X, Y, <) :-
  'rdf_list_<'(B, X, Y), !.
list_compare(B, X, Y, >) :-
  'rdf_list_>'(B, X, Y).

'rdf_list_<'(_, X, X) :- !.
'rdf_list_<'(B, X, Z) :-
  triple(B, L1, rdf:first, X),
  triple(B, L1, rdf:rest, L2),
  triple(B, L2, rdf:first, Y),
  'rdf_list_<'(B, Y, Z).

'rdf_list_>'(_, X, X) :- !.
'rdf_list_>'(B, X, Z) :-
  triple(B, L1, rdf:first, X),
  triple(B, L2, rdf:rest, L1),
  triple(B, L2, rdf:first, Y),
  'rdf_list_<'(B, Y, Z).



%! list_last(?Backend, ?L:rdf_list, ?X:rdf_term) is nondet.
%
% X is the last item in RDF list L.

list_last(B, L, X) :-
  triple(B, L, rdf:rest, T),
  (   rdf_equal(T, rdf:nil)
  ->  triple(B, L, rdf:first, X)
  ;   list_last(B, T, X)
  ).



%! list_last_triple(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?X:rdf_term) is nondet.

list_last_triple(B, S, P, X) :-
  list_triple(B, S, P, L),
  list_last(B, L, X).



%! list_member(?Backend, ?X:rdf_term, ?L:rdf_list) is nondet.

list_member(B, X, L) :-
  triple(B, L, rdf:first, X).
list_member(B, X, L) :-
  triple(B, L, rdf:rest, T),
  list_member(B, X, T).



%! list_member_triple(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?X:rdf_term) is nondet.

list_member_triple(B, S, P, X) :-
  ground(X), !,
  list_member(B, X, L),
  triple(B, S, P, L).
list_member_triple(B, S, P, X) :-
  triple(B, S, P, L),
  list_member(B, X, L).



%! list_triple(?Backend, ?S:rdf_subject, ?P:rdf_predicate, -PrologList:list(term)) is nondet.

list_triple(B, S, P, L2) :-
  triple(B, S, P, L1),
  list(B, L1, L2).



%! node(?Backend, ?Node:rdf_term) is nondet.

% subject node
node(B, S) :-
  triple(B, S, _, _).
% object node
node(B, O) :-
  triple(B, _, _, O).



%! reification(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object, -Statement:iri) is nondet.

reification(B, S, P, O, Stmt) :-
  triple(B, Stmt, rdf:subject, S),
  triple(B, Stmt, rdf:predicate, P),
  triple(B, Stmt, rdf:object, O).



%! retractall_triples(+Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object) is det.

retractall_triples(B, S, P, O) :-
  rdf_api:retractall_triples_(B, S, P, O).

rdf_api:retractall_triples_(dummy, _, _, _) :-
  fail.



%! strict_subclass(?Backend, +C:rdf_class, +D:rdf_class) is semidet.
%! strict_subclass(?Backend, +C:rdf_class, -D:rdf_class) is nondet.
%! strict_subclass(?Backend, -C:rdf_class, +D:rdf_class) is nondet.

strict_subclass(_, C, D) :-
  maplist(var, [C,D]), !,
  instantiation_error(args([C,D])).
strict_subclass(B, C, D) :-
  path_closure(subclass_(B), C, D).



%! subclass(?Backend, +C:rdf_class, +D:rdf_class) is semidet.
%! subclass(?Backend, +C:rdf_class, -D:rdf_class) is multi.
%! subclass(?Backend, -C:rdf_class, +D:rdf_class) is multi.

subclass(_, C, D) :-
  maplist(var, [C,D]), !,
  instantiation_error(args([C,D])).
subclass(B, C, D) :-
  path_closure0(subclass_(B), C, D).

subclass_(B, C, D) :-
  triple(B, C, rdfs:subClassOf, D).



%! term_string(+Backend, +Term:rdf_term, -String:string) is det.

term_string(B, S, String) :-
  triple_chk(B, S, rdfs:label, Literal), !,
  term_string(B, Literal, String).
term_string(_, Name, String) :-
  rdf_name_string(Name, String).



%! triple(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object) is nondet.

triple(B, S, P1, O1) :-
  (var(P1) -> P2 = P1 ; rdf_predicate_dwim(P1, P2)),
  (var(O1) -> O2 = O1 ; rdf_object_dwim(O1, O2)),
  rdf_api:triple_(B, S, P2, O2).

rdf_api:triple_(dummy, _, _, _) :-
  fail.



%! triple_chk(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object) is nondet.

triple_chk(B, S, P, O) :-
  once(triple(B, S, P, O)).



%! triple_count(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object, -N:nonneg) is nondet.

triple_count(B, S, P, O, N) :-
  rdf_api:triple_count_(B, S, P, O, N).

rdf_api:triple_count_(dummy, _, _, _, 0) :-
  fail.
