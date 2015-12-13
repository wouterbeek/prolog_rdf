:- module(
  id_store,
  [
    assign_id/1, % +Term
    assign_id/2, % +Term:rdf_term
                 % -Id:atom
    id_terms/1, % -Terms:ordset(rdf_term)
    id_to_term/2, % +Id:or([rdf_literal,uid])
                  % -Term:rdf_term
    id_to_terms/2, % +Id:or([rdf_literal,uid])
                   % -Terms:ordset(rdf_term)
    print_raw_graph/1, % ?Graph, +Options
    print_raw_graph/2, % ?Graph:rdf_graph
                       % +Options:list(compound)
    print_raw_statement/4, % ?Subject, ?Predicate, ?Object, ?Graph
    print_raw_statement/5, % ?Subject:rdf_term
                           % ?Predicate:iri
                           % ?Object:rdf_term
                           % ?Graph:rdf_graph
                           % +Options:list(compound)
    print_store/0,
    print_store/1, % +Options:list(compound)
    rdf_is_id/2, % +Term1:rdf_term
                 % +Term2:rdf_term
    remove_id/1, % +Id:or([rdf_literal,uid])
    store_id/2, % +Id1:or([rdf_literal,uid])
                % +Id2:or([rdf_literal,uid])
    term_to_id/2, % +Term:rdf_term
                  % -Id:or([rdf_literal,uid])
    term_to_term/2, % +Term1:rdf_term
                    % -Term2:rdf_term
    term_to_terms/2, % +Term:rdf_term
                     % -Terms:ordset(rdf_term)
    unload_id_store/0
  ]
).

/** <module> Identity store

RDF terms are mapped to/from identity set identifiers.

RDF terms are either blank nodes, IRIs or literals.

Identity set identifiers are either UIDs or literals.

A UID identity set identifier denotes a set of blank nodes and/or IRIs.

A literal identity set identifier denotes itself.

@author Wouter Beek
@version 2015/08, 2015/10, 2015/12
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
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_print_term)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db), [
     rdf/3 as rdf0,
     rdf/4 as rdf0,
     rdf_assert/4 as rdf_assert0,
     rdf_retractall/4 as rdf_retractall0
   ]).

:- rdf_meta(assign_id(o)).
:- rdf_meta(assign_id(o,-)).
:- rdf_meta(print_raw_graph(r)).
:- rdf_meta(print_raw_graph(r,+)).
:- rdf_meta(print_raw_statement(o,r,o,r)).
:- rdf_meta(print_raw_statement(o,r,o,r,+)).
:- rdf_meta(rdf_is_id(r,r)).
:- rdf_meta(store_id(o,o)).
:- rdf_meta(term_to_id(o,-)).
:- rdf_meta(term_to_term(o,-)).
:- rdf_meta(term_to_terms(o,-)).

%! id_to_terms(+Id:uid, -Terms:ordset(or([rdf_bnode,iri]))) is nondet.

:- dynamic(id_to_terms/2).

%! term_to_id(+Term:or([rdf_bnode,iri]), -Id:uid) is nondet.

:- dynamic(term_to_id/2).

:- predicate_options(print_raw_graph/2, 2, [
     pass_to(print_raw_statement/5, 5)
   ]).
:- predicate_options(print_raw_statement/5, 5, [
     indent(+nonneg)
   ]).
:- predicate_options(print_store/1, 1, [
     pass_to(rdf_print_term//2, 2)
   ]).

:- initialization((rdf_expand_ct(owl:sameAs, P), assign_id(P))).





%! assign_id(+Term:rdf_term) is det.
% Wrapper around assign_id/2 that does not return the identifier.

assign_id(T):-
  assign_id(T, _).


%! assign_id(+Term:rdf_term, -Id:atom) is det.

assign_id(T, TId):-
  canonical_form(T, CT),
  (   term_to_id(CT, TId)
  ->  true
  ;   create_id(TId),
      with_mutex(id_store, store_new_id(CT, TId))
  ).

canonical_form(Lit, CLit):-
  rdf_is_literal(Lit), !,
  rdf_lexical_canonical_map(Lit, CLit).
canonical_form(T, T).



%! id_terms(-Terms:ordset(rdf_term)) is nondet.
% Allows identical terms to be enumerated.

id_terms(Ts):- id_to_terms(_, Ts).



%! id_to_term(+Id:uid, +Term:rdf_term) is multi.

id_to_term(Id, T):- id_to_terms(Id, Ts), member(T, Ts).



%! print_store is det.
% Wrapper around print_store/1 with default options.

print_store:- print_store([]).



%! print_raw_graph(?Graph:rdf_graph) is det.
% Wrapper around print_raw_graph/2 with default options.

print_raw_graph(G):- print_raw_graph(G, []).


%! print_raw_graph(?Graph:rdf_graph, +Options:list(compound)) is det.

print_raw_graph(G, Opts):-
  defval(default, G),
  print_raw_statement(_, _, _, _, Opts),
  fail.
print_raw_graph(_, _).



%! print_raw_statement(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:rdf_graph
%! ) is nondet.
% Wrapper around print_raw_statement/5 with default options.

print_raw_statement(S, P, O, G):-
  print_raw_statement(S, P, O, G, []).


%! print_raw_statement(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:rdf_graph,
%!   +Options:list(compound)
%! ) is nondet.

print_raw_statement(S, P, O, G, Opts):-
  rdf0(S, P, O, G),
  option(indent(I), Opts, 0), tab(I),
  dcg_with_output_to(current_output, print_raw_statement(S, P, O, G)),
  nl.


print_raw_statement(S, P, O, G) -->
  triple(atom, [S,P,O]),
  ({var(G)} -> "" ; rdf_print_graph(G)).



%! print_store(+Options:list(compound)) is det.
% The following options are supported:
%   * indent(+nonneg)
%     Default is 0.

print_store(Opts):-
  option(indent(N), Opts, 0),
  forall(
    id_to_terms(Id, Ts),
    dcg_with_output_to(user_output, (
      tab(N),
      atom(Id),
      "\t",
      set(rdf_print_term0(Opts), Ts),
      nl
   ))
  ).
rdf_print_term0(Opts, T) --> rdf_print_term(T, Opts).



%! rdf_is_id(+Term1:rdf_term, +Term2:rdf_term) is semidet.

rdf_is_id(T1, T2):-
  term_to_terms(T1, Ts),
  memberchk(T2, Ts).



%! remove_id(+Id:uid) is det.

remove_id(Id):-
  with_mutex(id_store, (
    retractall(id_to_terms(Id,_)),
    retractall(term_to_id(_,Id))
  )).




%! store_id(+X:rdf_term, +Y:rdf_term) is det.

store_id(X, Y):-
  (   term_to_id(X, Xid)
  ->  (   term_to_id(Y, Yid)
      ->  (   % X and Y already belong to the same identity set.
              Xid == Yid
          ->  true
          ;   % Merge the identity sets of X and Y.
              id_to_terms(Xid, Xs),
              id_to_terms(Yid, Ys),
              ord_union(Xs, Ys, Zs),
              create_id(Zid),
              with_mutex(id_store, (
                retract(term_to_id(X, Xid)),
                retract(id_to_terms(Xid, Xs)),
                retract(term_to_id(Y, Yid)),
                retract(id_to_terms(Yid, Ys)),
                assert(term_to_id(X, Zid)),
                assert(term_to_id(Y, Zid)),
                assert(id_to_terms(Zid, Zs)),
                rdf_rename_term0(Xid, Zid),
                rdf_rename_term0(Yid, Zid)
             ))
          )
      ;   % Add Y to the identity set of X.
          id_to_terms(Xid, Xs1),
          ord_add_element(Xs1, Y, Xs2),
          with_mutex(id_store, (
            retract(id_to_terms(Xid, Xs1)),
            assert(id_to_terms(Xid, Xs2)),
            assert(term_to_id(Y, Xid))
          ))
      )
  ;   term_to_id(Y, Yid)
  ->  % Add X to the identity set of Y.
      id_to_terms(Yid, Ys1),
      ord_add_element(Ys1, X, Ys2),
      with_mutex(id_store, (
        retract(id_to_terms(Yid, Ys1)),
        assert(id_to_terms(Yid, Ys2)),
        assert(term_to_id(X, Yid))
      ))
  ;   create_id(Xid),
      create_id(Yid),
      with_mutex(id_store, (
        store_new_id(X, Xid),
        store_new_id(Y, Yid)
      ))
  ).



%! term_to_term(+Term1:rdf_term, -Term2:rdf_term) is multi.

term_to_term(T1, T2):- term_to_terms(T1, Ts), member(T2, Ts).



%! term_to_terms(+Term:rdf_term, -Terms:ordset(rdf_term)) is det.

term_to_terms(T, Ts):- term_to_id(T, Id), id_to_terms(Id, Ts).



%! unload_id_store is det.

unload_id_store:-
  retractall(id_to_terms(_,_,_)),
  retractall(term_to_id(_,_)).





% HELPERS %

%! create_id(-Id:nonneg) is det.

create_id(Id):- flag(id_counter, N, N + 1), atom_number(Id, N).



%! rdf_rename_term0(+From, +To) is det.

rdf_rename_term0(X, Y):-
  forall(rdf0(X, P, O, G), rdf_assert0(Y, P, O, G)),
  rdf_retractall0(X, _, _, _),
  forall(rdf0(S, X, O, G), rdf_assert0(S, Y, O, G)),
  rdf_retractall0(_, X, _, _),
  forall(rdf0(S, P, X, G), rdf_assert0(S, P, Y, G)),
  rdf_retractall0(_, _, X, _).



%! store_new_id(+Term:rdf_term, +Id:uid) is det.

store_new_id(T, TId):-
  assert(term_to_id(T, TId)),
  assert(id_to_terms(TId, [T])).
