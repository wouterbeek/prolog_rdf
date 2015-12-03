:- module(
  id_store,
  [
    id_assign_literal/2, % ?Literal:rdf_literal
                         % ?Id:uid
    id_assign_term/2, % ?Term:rdf_term
                      % ?Id:or([rdf_literal,uid])
    id_to_literal/2, % ?Id:uid
                     % ?Literal:rdf_literal
    id_terms/1, % ?Terms:ordset(rdf_term)
    id_to_term/2, % +Id:or([rdf_literal,uid])
                  % -Term:rdf_term
    id_to_terms/2, % +Id:or([rdf_literal,uid])
                   % -Terms:ordset(rdf_term)
    id_print_store/0,
    id_print_store/1, % +Options:list(compound)
    id_remove/1, % +Id:or([rdf_literal,uid])
    id_store/1, % +Terms:list(rdf_term)
    id_store/2, % +Term1:rdf_term
                % +Term2:rdf_term
    literal_to_id/2, % +Literal:rdf_literal
                     % -Id:or([rdf_literal,uid])
    term_to_id/2, % +Term:rdf_term
                  % -Id:or([rdf_literal,uid])
    term_to_term/2, % +Term1:rdf_term
                    % -Term2:rdf_term
    term_to_terms/2, % +Term:rdf_term
                     % -Terms:ordset(rdf_term)
    var_or_term_to_id/2 % ?Term:rdf_term
                        % ?Id:or([rdf_literal,uid])
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
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_print)).

:- rdf_meta(id_assign_term(o,?)).
:- rdf_meta(id_terms(t)).
:- rdf_meta(id_to_literal(+,-)).
:- rdf_meta(id_store(t)).
:- rdf_meta(id_store(o,o)).
:- rdf_meta(literal_to_id(o,-)).
:- rdf_meta(term_to_id(o,-)).
:- rdf_meta(term_to_term(o,-)).
:- rdf_meta(term_to_terms(o,-)).
:- rdf_meta(var_or_term_to_id(o,?)).

%! id_to_literal(?Literal:rdf_literal, ?Id:uid) is nondet.

:- dynamic(id_to_literal/2).

%! id_terms0(?Id:uid, ?Terms:ordset(or([rdf_bnode,iri]))) is nondet.

:- dynamic(id_terms0/2).

%! term_id0(?Term:or([rdf_bnode,iri]), Id:uid) is nondet.

:- dynamic(term_id0/2).

:- predicate_options(print_id_store/1, 1, [
     pass_to(rdf_print_term//2, 2)
   ]).





%! id_assign_literal(+Literal:rdf_literal, -Id:uid) is det.

id_assign_literal(Lit, Id):-
  id_to_literal(Lit, Id), !.
id_assign_literal(Lit, Id):-
  rdf_bnode(BNode),
  id_assign_term(BNode, Id),
  assert(id_literal(Lit, Id)).



%! id_assign_term(?Term:rdf_term, -Id:uid) is det.

id_assign_term(T, TId):-
  var_or_term_to_id(T, TId), !.
id_assign_term(T, TId):-
  fresh_id(TId),
  with_mutex(id_store, store_term0(T, TId)).



%! id_terms(+Terms:ordset(rdf_term)) is semidet.
%! id_terms(-Terms:ordset(rdf_term)) is nondet.

id_terms(Ts1):-
  nonvar(Ts1), !,
  memberchk(T, Ts1),
  term_id0(T, TId),
  id_terms0(TId, Ts2),
  ord_subset(Ts1, Ts2).
id_terms(Ts):-
  id_terms0(_, Ts).



%! id_to_term(+Id:uid, +Term:rdf_term) is multi.

id_to_term(Id, T):-
  nonvar(Id), !,
  id_to_terms(Id, Ts),
  member(T, Ts).
id_to_term(Id, T):-
  nonvar(T), !,
  term_id0(T, Id).
id_to_term(Id, T):-
  instantiation_error(id_term(Id,T)).



%! id_to_terms(+Id:uid, -Terms:ordset(rdf_term)) is multi.

id_to_terms(Id, Ts):-
  id_terms0(Id, Ts).



%! id_print_store is det.
% Wrapper around id_print_store/1 with default options.

id_print_store:-
  id_print_store([]).


%! id_print_store(+Options:list(compound)) is det.
% The following options are supported:
%   * indent(+nonneg)
%     Default is 0.

id_print_store(Opts):-
  option(indent(N), Opts, 0),
  forall(
    id_terms0(Id, Ts),
    dcg_with_output_to(user_output, (
      tab(N),
      atom(Id),
      "\t",
      set(\T^rdf_print_term(T, Opts), Ts),
      nl
   ))
  ).



%! id_remove(+Id:uid) is det.

id_remove(Id):-
  with_mutex(id_store, (
    retractall(id_terms0(Id,_)),
    retractall(term_id0(_,Id))
  )).




%! id_store(+Terms:list(rdf_term)) is det.

id_store([H1,H2|T]):-
  id_store(H1, H2), !,
  id_store([H2|T]).
id_store([_]):- !.
id_store([]).


%! id_store(+X:rdf_term, +Y:rdf_term) is det.

id_store(X, X):-
  rdf_is_literal(X).
id_store(X, Y):-
  (   term_id0(X, XId)
  ->  (   term_id0(Y, YId)
      ->  (   % X and Y already belong to the same identity set.
              XId == YId
          ->  true
          ;   % Merge the identity sets of X and Y.
              id_terms0(XId, Xs),
              id_terms0(YId, Ys),
              ord_union(Xs, Ys, Zs),
              fresh_id(ZId),
              with_mutex(id_store, (
                retract(term_id0(X, XId)),
                retract(id_terms0(XId, Xs)),
                retract(term_id0(Y, YId)),
                retract(id_terms0(YId, Ys)),
                assert(term_id0(X, ZId)),
                assert(term_id0(Y, ZId)),
                assert(id_terms0(ZId, Zs)),
                rdf_rename_term(XId, ZId),
                rdf_rename_term(YId, ZId)
             ))
          )
      ;   % Add Y to the identity set of X.
          id_terms0(XId, Xs1),
          ord_add_element(Xs1, Y, Xs2),
          with_mutex(id_store, (
            retract(id_terms0(XId, Xs1)),
            assert(id_terms0(XId, Xs2)),
            assert(term_id0(Y, XId))
          ))
      )
  ;   term_id0(Y, YId)
  ->  % Add X to the identity set of Y.
      id_terms0(YId, Ys1),
      ord_add_element(Ys1, X, Ys2),
      with_mutex(id_store, (
        retract(id_terms0(YId, Ys1)),
        assert(id_terms0(YId, Ys2)),
        assert(term_id0(X, YId))
      ))
  ;   fresh_id(XId),
      fresh_id(YId),
      with_mutex(id_store, (
        store_term0(X, XId),
        store_term0(Y, YId)
      ))
  ).



literal_to_id(Lit, Id):-
  id_to_literal(Id, Lit).



%! term_to_id(+Term:rdf_term, -Id:or([rdf_literal,uid])) is det.

term_to_id(Lit, Lit):-
  rdf_is_literal(Lit), !.
term_to_id(T, Id):-
  term_id0(T, Id).



%! term_to_term(+Term1:rdf_term, -Term2:rdf_term) is multi.

term_to_term(T1, T2):-
  term_to_terms(T1, Ts),
  member(T2, Ts).



%! term_to_terms(+Term:rdf_term, -Terms:ordset(rdf_term)) is det.

term_to_terms(T, Ts):-
  term_to_id(T, Id),
  id_to_terms(Id, Ts).



%! var_or_term_to_id(?Term:rdf_term, ?Id:or([rdf_literal,uid])) is det.

var_or_term_to_id(T, _):-
  var(T), !.
var_or_term_to_id(T, TId):-
  term_to_id(T, TId).





% HELPERS %

%! fresh_id(-Id:nonneg) is det.

fresh_id(Id):-
  flag(id_counter, N, N + 1),
  atom_number(Id, N).



%! rdf_rename_term(+From, +To) is det.

rdf_rename_term(X, Y):-
  forall(rdf(X, P, O), rdf_assert(Y, P, O)),
  grdf_retractall(X, _, _),
  forall(rdf(S, X, O), rdf_assert(S, Y, O)),
  grdf_retractall(_, X, _),
  forall(rdf(S, P, X), rdf_assert(S, P, Y)),
  grdf_retractall(_, _, X).
  


%! store_term0(+Term:or([rdf_bnode,iri]), +Id:uid) is det.

store_term0(T, TId):-
  assert(term_id0(T, TId)),
  assert(id_terms0(TId, [T])).
