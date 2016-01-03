:- module(
  id_store,
  [
    assign_graph_id/2,		% +G, -Gid
    assign_id/1,		% +T
    assign_id/2,		% +T, -Tid
    check_id_store/0,
    dangling_id/1,		% ?Tid
    graph_id_to_term/2,		% +Gid, -G
    graph_term_to_id/2,		% +G, -Gid
    id_terms/1,			% -Ts:ordset
    id_to_term/2,		% +Tid, -T
    id_to_terms/2,		% +Tid, -Ts:ordset
    print_store/0,
    print_store/1,		% +Opts
    rdf_is_id/2,		% +T1, +T2
    remove_id/1,		% +Tid
    store_id/2,			% +Tid1, +Tid2
    term_to_id/2,		% +T, -Tid
    term_to_term/2,		% +T1, -T2
    term_to_terms/2,		% +T, -Ts
    unload_id_store/0
  ]
).

/** <module> Identity store

RDF terms are mapped to/from identity set identifiers.

RDF terms are either blank nodes, IRIs or literals.

Identity set identifiers are either UIDs or literals.

A UID identity set identifier denotes a set of blank nodes and/or IRIs.

A literal identity set identifier denotes itself.

Term identifiers are denoted `Tid'.
Subject term identifiers are denoted `Sid'.
Predicate term identifiers are denoted `Pid'.
Object term identifiers are denoted `Oid'.
Graph identifiers are defnoted `Gif'.
Arbitrary identifiers are denoted `Xid', `Yid' and `Zid'.
Identifiers are atoms.

---

@author Wouter Beek
@version 2015/08, 2015/10, 2015/12-2016/01
*/

:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(typecheck)).
:- use_module(library(uri)).

:- rdf_meta
	assign_graph_id(r, -),
	assign_id(o),
	assign_id(o, -),
	graph_term_to_id(r, -),
	rdf_is_id(r, r),
	store_id(o, o),
	term_to_id(o, -),
	term_to_term(o, -),
	term_to_terms(o, -).

%! id_to_terms0(+Tid, -Ts:ordset) is nondet.

:- dynamic(id_to_terms0/2).

%! term_to_id0(+T, -Tid) is nondet.

:- dynamic(term_to_id0/2).

:- predicate_options(print_store/1, 1, [
     pass_to(rdf_print_term//2, 2)
   ]).

:- initialization((rdf_equal(owl:sameAs, P), assign_id(P))).





%! assign_graph_id(+G, -Gid) is det.

assign_graph_id(default, default) :- !.
assign_graph_id(G, Gid) :-
  assign_id(G, Gid).



%! assign_id(+T) is det.
% Wrapper around assign_id/2 that does not return the identifier.

assign_id(T) :-
  assign_id(T, _).


%! assign_id(+T, -Tid) is det.

assign_id(T, Tid) :-
  canonical_form(T, CT),
  with_mutex(id_store, (
    (   term_to_id0(CT, Tid)
    ->  true
    ;   create_id(Tid),
        store_new_id0(CT, Tid)
    )
  )).

canonical_form(B, B) :-
  rdf_is_bnode(B), !.
canonical_form(Lit, CLit) :-
  rdf_is_literal(Lit), !,
  rdf_lexical_canonical_map(Lit, CLit).
canonical_form(Iri, Norm) :-
  rdf_is_iri(Iri), !,
  iri_normalized(Iri, Norm).
canonical_form(BN, BN).



%! check_id_store is semidet.

check_id_store:-
  \+ dangling_id(_), !.
check_id_store:-
  format(user_output, "Dangling IDs:~n", []),
  forall(distinct(Tid, dangling_id(Tid)), format(user_error, "  ~a~n", [Tid])),
  fail.



%! dangling_id(+Tid) is semidet.
%! dangling_id(-Tid) is nondet.

dangling_id(Id) :-  
  term_to_id(_, Id),
  \+ id_to_terms(Id, _).



%! graph_id_to_term(+Gid, -G) is det.

graph_id_to_term(default, default) :- !.
graph_id_to_term(Gid, G) :-
  id_to_term(Gid, G).



%! graph_term_to_id(+G, -Gid) is det.

graph_term_to_id(default, default) :- !.
graph_term_to_id(G, Gid) :-
  term_to_id(G, Gid).



%! id_terms(-Ts:ordset) is nondet.
% Allows identical terms to be enumerated.

id_terms(Ts) :-
  id_to_terms(_, Ts).



%! id_to_term(+Tid, -T) is multi.

id_to_term(Id, T) :-
  id_to_terms(Id, Ts),
  member(T, Ts).



%! id_to_terms(+Tid, -Ts:ordset) is det.

id_to_terms(Tid, Ts) :-
  id_to_terms0(Tid, Ts).



%! print_store is det.
% Wrapper around print_store/1 with default options.

print_store:-
  print_store([]).


%! print_store(+Opts) is det.
% The following options are supported:
%   * indent(+nonneg)
%     Default is 0.

print_store(Opts) :-
  option(indent(N), Opts, 0),
  forall(
    id_to_terms(Tid, Ts),
    dcg_with_output_to(user_output, (
      tab(N),
      atom(Tid),
      "\t",
      set(rdf_print_term0(Opts), Ts),
      nl
   ))
  ).
rdf_print_term0(Opts, T) --> rdf_print_term(T, Opts).



%! rdf_is_id(+T1, +T2) is semidet.

rdf_is_id(T1, T2) :-
  term_to_terms(T1, Ts),
  memberchk(T2, Ts).



%! remove_id(+Tid) is det.

remove_id(Tid) :-
  with_mutex(id_store, (
    retractall(id_to_terms0(Tid,_)),
    retractall(term_to_id0(_,Tid))
  )).




%! store_id(+X, +Y) is det.

store_id(X, Y) :-
  with_mutex(id_store, store_id0(X, Y)).
store_id0(X, Y) :-
  (   term_to_id0(X, Xid)
  ->  (   term_to_id0(Y, Yid)
      ->  (   % X and Y already belong to the same identity set.
              Xid == Yid
          ->  true
          ;   % Merge the identity sets of X and Y.
              id_to_terms0(Xid, Xs),
              id_to_terms0(Yid, Ys),
              ord_union(Xs, Ys, Zs),
              create_id(Zid),
	      forall(member(X0, Xs), retract(term_to_id0(X0, Xid))),
              retract(id_to_terms0(Xid, Xs)),
              forall(member(Y0, Ys), retract(term_to_id0(Y0, Yid))),
              retract(id_to_terms0(Yid, Ys)),
              assert(term_to_id0(X, Zid)),
              assert(term_to_id0(Y, Zid)),
              assert(id_to_terms0(Zid, Zs)),
              rdf_rename_term0(Xid, Zid),
              rdf_rename_term0(Yid, Zid)
          )
      ;   % Add Y to the identity set of X.
          id_to_terms0(Xid, Xs1),
          ord_add_element(Xs1, Y, Xs2),
          retract(id_to_terms0(Xid, Xs1)),
          assert(id_to_terms0(Xid, Xs2)),
          assert(term_to_id0(Y, Xid))
      )
  ;   term_to_id0(Y, Yid)
  ->  % Add X to the identity set of Y.
      id_to_terms0(Yid, Ys1),
      ord_add_element(Ys1, X, Ys2),
      retract(id_to_terms0(Yid, Ys1)),
      assert(id_to_terms0(Yid, Ys2)),
      assert(term_to_id0(X, Yid))
  ;   create_id(Xid),
      create_id(Yid),
      store_new_id0(X, Xid),
      store_new_id0(Y, Yid)
  ).



%! term_to_id(+T, -Tid) is semidet.

term_to_id(default, default) :- !.
term_to_id(T1, Tid) :-
  (rdf_is_iri(T1) -> iri_normalized(T1, T2) ; T2 = T1),
  term_to_id0(T2, Tid).



%! term_to_term(+T1, -T2) is multi.

term_to_term(T1, T2) :-
  term_to_terms(T1, Ts),
  member(T2, Ts).



%! term_to_terms(+T, -Ts:ordset) is det.

term_to_terms(T, Ts) :-
  term_to_id(T, Tid),
  id_to_terms(Tid, Ts).



%! unload_id_store is det.

unload_id_store:-
  with_mutex(id_store, (
    retractall(id_to_terms0(_,_,_)),
    retractall(term_to_id0(_,_))
  )).





% HELPERS %

%! create_id(-Tid:nonneg) is det.

create_id(Tid) :-
  flag(id_counter, N, N + 1),
  atom_number(Tid, N).



%! rdf_rename_term0(+Xid, +Yid) is det.

rdf_rename_term0(Xid, Yid) :-
  % Firstly. rename the graph `Xid', if it exists.
  (   rdf_is_graph(Xid)
  ->  forall(rdf_id(Sid, Pid, Oid, Xid), rdf_assert_id(Sid, Pid, Oid, Yid)),
      rdf_unload_graph(Xid)
  ;   true
  ),

  % Secondly, rename statements containing term X
  % in the subject, predicate and object position.
  forall(rdf_id(Xid, Pid, Oid, Gid), rdf_assert_id(Yid, Pid, Oid, Gid)),
  rdf_retractall_id(Xid, _, _, _),
  forall(rdf_id(Sid, Xid, Oid, Gid), rdf_assert_id(Sid, Yid, Oid, Gid)),
  rdf_retractall_id(_, Xid, _, _),
  forall(rdf_id(Sid, Pid, Xid, Gid), rdf_assert_id(Sid, Pid, Yid, Gid)),
  rdf_retractall_id(_, _, Xid, _).



%! store_new_id0(+T, +Tid) is det.

store_new_id0(T, Tid) :-
  assert(term_to_id0(T, Tid)),
  assert(id_to_terms0(Tid, [T])).
