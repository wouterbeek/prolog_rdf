:- module(
  id_store,
  [
    assign_literal_id/2, % +Literal:rdf_literal
                         % ?Id:uid
    assign_term_id/2, % +Term:rdf_term
                      % ?Id:or([rdf_literal,uid])
    id_terms/1, % -Terms:ordset(rdf_term)
    id_to_term/2, % +Id:or([rdf_literal,uid])
                  % -Term:rdf_term
    id_to_terms/2, % +Id:or([rdf_literal,uid])
                   % -Terms:ordset(rdf_term)
    literal_id/2, % ?Literal:rdf_literal
                  % ?Id:uid
    print_store/0,
    print_store/1, % +Options:list(compound)
    remove_id/1, % +Id:or([rdf_literal,uid])
    store_id/2, % +Id1:or([rdf_literal,uid])
                % +Id2:or([rdf_literal,uid])
    term_to_id/2, % +Term:rdf_term
                  % -Id:or([rdf_literal,uid])
    term_to_term/2, % +Term1:rdf_term
                    % -Term2:rdf_term
    term_to_terms/2 % +Term:rdf_term
                    % -Terms:ordset(rdf_term)
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
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db), [
     rdf/3 as rdf0,
     rdf_assert/3 as rdf_assert0,
     rdf_retractall/3 as rdf_retractall0
   ]).

:- rdf_meta(assign_literal_id(o,-)).
:- rdf_meta(assign_term_id(o,?)).
:- rdf_meta(literal_id(o,-)).
:- rdf_meta(store_id(o,o)).
:- rdf_meta(term_to_id(o,-)).
:- rdf_meta(term_to_term(o,-)).
:- rdf_meta(term_to_terms(o,-)).

%! id_to_terms0(+Id:uid, -Terms:ordset(or([rdf_bnode,iri]))) is nondet.

:- dynamic(id_to_terms0/2).

%! literal_id(?Literal:rdf_literal, ?Id:uid) is nondet.

:- dynamic(literal_id/2).

%! term_to_id0(+Term:or([rdf_bnode,iri]), -Id:uid) is nondet.

:- dynamic(term_to_id0/2).

:- predicate_options(print_store/1, 1, [
     pass_to(rdf_print_term//2, 2)
   ]).





%! assign_literal_id(+Literal:rdf_literal, -Id:uid) is det.

% Literal was already assigned an identifier.
assign_literal_id(Lit, Id):-
  literal_id(Lit, Id), !.
% Literal is assigned a new identifier.
assign_literal_id(Lit, Id):-
  rdf_bnode(BNode),
  assign_term_id(BNode, Id),
  assert(literal_id(Lit, Id)).



%! assign_term_id(+Term:rdf_term, -Id:uid) is det.

assign_term_id(T, TId):-
  term_to_id0(T, TId), !.
assign_term_id(T, TId):-
  create_id(TId),
  with_mutex(id_store, store_new_id(T, TId)).



%! id_terms(-Terms:ordset(rdf_term)) is nondet.
% Allows identical terms to be enumerated.

id_terms(Ts):-
  id_to_terms0(_, Ts).



%! id_to_term(+Id:uid, +Term:rdf_term) is multi.

id_to_term(Id, T):-
  id_to_terms(Id, Ts),
  % MULTI
  member(T, Ts).



%! id_to_terms(+Id:uid, -Terms:ordset(rdf_term)) is multi.

id_to_terms(Id, Ts):-
  id_to_terms0(Id, Ts).



%! print_store is det.
% Wrapper around print_store/1 with default options.

print_store:-
  print_store([]).


%! print_store(+Options:list(compound)) is det.
% The following options are supported:
%   * indent(+nonneg)
%     Default is 0.

print_store(Opts):-
  option(indent(N), Opts, 0),
  forall(
    id_to_terms0(Id, Ts),
    dcg_with_output_to(user_output, (
      tab(N),
      atom(Id),
      "\t",
      set(rdf_print_term0(Opts), Ts),
      nl
   ))
  ).
rdf_print_term0(Opts, T) --> rdf_print_term(T, Opts).



%! remove_id(+Id:uid) is det.

remove_id(Id):-
  with_mutex(id_store, (
    retractall(id_to_terms0(Id,_)),
    retractall(term_to_id0(_,Id))
  )).




%! store_id(+X:rdf_term, +Y:rdf_term) is det.

% At least one of the terms is a literal.
store_id(X, Y1):-
  (rdf_is_literal(X) ; rdf_is_literal(Y1)), !,
  (   rdf_is_literal(X)
  ->  assign_literal_id(X, XId)
  ;   assign_term_id(X, XId)
  ),
  (   rdf_is_literal(Y1)
  ->  Y2 = Y1
  ;   assign_term_id(Y1, Y2)
  ),
  rdf_assert(XId, owl:sameAs, Y2).
% Neither term is a literal.
id_store(X, Y):-
  (   term_to_id0(X, XId)
  ->  (   term_to_id0(Y, YId)
      ->  (   % X and Y already belong to the same identity set.
              XId == YId
          ->  true
          ;   % Merge the identity sets of X and Y.
              id_to_terms0(XId, Xs),
              id_to_terms0(YId, Ys),
              ord_union(Xs, Ys, Zs),
              create_id(ZId),
              with_mutex(id_store, (
                retract(term_to_id0(X, XId)),
                retract(id_to_terms0(XId, Xs)),
                retract(term_to_id0(Y, YId)),
                retract(id_to_terms0(YId, Ys)),
                assert(term_to_id0(X, ZId)),
                assert(term_to_id0(Y, ZId)),
                assert(id_to_terms0(ZId, Zs)),
                rdf_rename_term0(XId, ZId),
                rdf_rename_term0(YId, ZId)
             ))
          )
      ;   % Add Y to the identity set of X.
          id_to_terms0(XId, Xs1),
          ord_add_element(Xs1, Y, Xs2),
          with_mutex(id_store, (
            retract(id_to_terms0(XId, Xs1)),
            assert(id_to_terms0(XId, Xs2)),
            assert(term_to_id0(Y, XId))
          ))
      )
  ;   term_to_id0(Y, YId)
  ->  % Add X to the identity set of Y.
      id_to_terms0(YId, Ys1),
      ord_add_element(Ys1, X, Ys2),
      with_mutex(id_store, (
        retract(id_to_terms0(YId, Ys1)),
        assert(id_to_terms0(YId, Ys2)),
        assert(term_to_id0(X, YId))
      ))
  ;   create_id(XId),
      create_id(YId),
      with_mutex(id_store, (
        store_new_id(X, XId),
        store_new_id(Y, YId)
      ))
  ).



%! term_to_id(+Term:rdf_term, -Id:or([rdf_literal,uid])) is det.

term_to_id(Lit, Lit):-
  rdf_is_literal(Lit), !.
term_to_id(T, Id):-
  term_to_id0(T, Id).



%! term_to_term(+Term1:rdf_term, -Term2:rdf_term) is multi.

term_to_term(T1, T2):-
  term_to_terms(T1, Ts),
  member(T2, Ts).



%! term_to_terms(+Term:rdf_term, -Terms:ordset(rdf_term)) is det.

term_to_terms(T, Ts):-
  term_to_id(T, Id),
  id_to_terms(Id, Ts).





% HELPERS %

%! create_id(-Id:nonneg) is det.

create_id(Id):-
  flag(id_counter, N, N + 1),
  atom_number(Id, N).



%! rdf_rename_term0(+From, +To) is det.

rdf_rename_term0(X, Y):-
  forall(rdf0(X, P, O), rdf_assert0(Y, P, O)),
  rdf_retractall0(X, _, _),
  forall(rdf0(S, X, O), rdf_assert0(S, Y, O)),
  rdf_retractall0(_, X, _),
  forall(rdf0(S, P, X), rdf_assert0(S, P, Y)),
  rdf_retractall0(_, _, X).
  


%! store_new_id(+Term:rdf_term, +Id:uid) is det.

store_new_id(T, TId):-
  assert(term_to_id0(T, TId)),
  assert(id_to_terms0(TId, [T])).
