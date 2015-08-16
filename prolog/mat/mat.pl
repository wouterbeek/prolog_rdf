:- module(
  mat,
  [
    mat/1, % +InputGraph:atom
    mat/2 % +InputGraph:atom
          % ?OutputGraph:atom
  ]
).

/** <module> OWL materialization

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(chr)).
:- use_module(library(deb_ext)).
:- use_module(library(error)).
:- use_module(library(mat/j_db)).
:- use_module(library(mat/mat_deb)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

:- set_prolog_flag(chr_toplevel_show_store, false).

:- chr_constraint('_allTypes'/2).
:- chr_constraint(error/2).
:- chr_constraint(inList/2).
:- chr_constraint(rdf_chr/3).





%! mat(+InputGraph:atom) is det.

mat(GIn):-
  mat(GIn, _).

%! mat(+InputGraph:atom, +OutputGraph:atom) is det.
%! mat(+InputGraph:atom, -OutputGraph:atom) is det.

mat(GIn, GOut):-
  var(GOut), !,
  atomic_list_concat([GIn,mat], '_', GOut),
  mat(GIn, GOut).
mat(GIn, GOut):-
  % Type checking.
  must_be(atom, GIn),
  (   rdf_graph(GIn)
  ->  true
  ;   existence_error(rdf_graph, GIn)
  ),

  % Debug message before.
  PrintOpts = [
    abbr_list(true),
    elip_lit(20),
    elip_ln(20),
    indent(1),
    logic_sym(true),
    style(turtle)
  ],
  debug(mat(_), 'BEFORE MATERIALIZATION:', []),
  if_debug(mat(_), rdf_print_graph(GIn, PrintOpts)),

  % Perform materialization.
  findall(rdf_chr(S,P,O), rdf(S,P,O,GIn), Ins),
  maplist(store_j0(axiom, []), Ins),
  maplist(call, Ins),
  % Check whether an inconsistent state was reached.
  (   find_chr_constraint(error(_,_))
  ->  forall(find_chr_constraint(error(R,Ps)), print_j(R, Ps, _, _))
  ;   findall(rdf(S,P,O), find_chr_constraint(rdf_chr(S,P,O)), Outs),
      maplist(rdf_assert0(GOut), Outs), !,
      % Debug message after successful materialization.
      debug(mat(_), 'AFTER MATERIALIZATION:', []),
      if_debug(mat(_), rdf_print_graph(GOut, PrintOpts))
  ).

store_j0(Rule, Ps, rdf_chr(S,P,O)):-
  store_j(Rule, Ps, rdf(S,P,O)).

rdf_assert0(G, rdf(S,P,O)):-
  rdf_assert(S, P, O, G).





% RULES %

idempotence @
      rdf_chr(S, P, O)
  \   rdf_chr(S, P, O)
  <=> debug(db(idempotence), 'idempotence', rdf(S,P,O))
    | true.

%owl-eq-ref @
%      rdf_chr(S, P, O)
%  ==> mat_deb(owl(eq(ref)), [
%        rdf(S, P, O)],
%        rdf(S, 'http://www.w3.org/2002/07/owl#sameAs', S))
%    | rdf_chr(S, 'http://www.w3.org/2002/07/owl#sameAs', S).

%owl-eq-ref1 @
%      rdf_chr(S, P, O)
%  ==> mat_deb(owl(eq(ref1)), [
%        rdf(S, P, O)],
%        rdf(P, 'http://www.w3.org/2002/07/owl#sameAs', P))
%    | rdf_chr(P, 'http://www.w3.org/2002/07/owl#sameAs', P).

%owl-eq-ref2 @
%      rdf_chr(S, P, O)
%  ==> mat_deb(owl(eq(ref2)), [
%        rdf(S, P, O)],
%        rdf(O, 'http://www.w3.org/2002/07/owl#sameAs', O))
%    | rdf_chr(O, 'http://www.w3.org/2002/07/owl#sameAs', O).

owl-eq-sym @
      rdf_chr(S, 'http://www.w3.org/2002/07/owl#sameAs', O)
  ==> mat_deb(owl(eq(sym)), [
        rdf(S, 'http://www.w3.org/2002/07/owl#sameAs', O)],
        rdf(O, 'http://www.w3.org/2002/07/owl#sameAs', S))
    | rdf_chr(O, 'http://www.w3.org/2002/07/owl#sameAs', S).

owl-eq-trans @
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Y),
      rdf_chr(Y, 'http://www.w3.org/2002/07/owl#sameAs', Z)
  ==> mat_deb(owl(eq(trans)), [
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Y),
        rdf(Y, 'http://www.w3.org/2002/07/owl#sameAs', Z)],
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Z))
    | rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Z).

owl-eq-rep-s @
      rdf_chr(S1, 'http://www.w3.org/2002/07/owl#sameAs', S2),
      rdf_chr(S1, P, O)
  ==> mat_deb(owl(eq(rep(s))), [
        rdf(S1, 'http://www.w3.org/2002/07/owl#sameAs', S2),
        rdf(S1, P, O)],
        rdf(S2, P, O))
    | rdf_chr(S2, P, O).

owl-eq-rep-p @
      rdf_chr(P1, 'http://www.w3.org/2002/07/owl#sameAs', P2),
      rdf_chr(S, P1, O)
  ==> mat_deb(owl(eq(rep(s))), [
        rdf(P1, 'http://www.w3.org/2002/07/owl#sameAs', P2),
        rdf(S, P1, O)],
        rdf(S, P2, O))
    | rdf_chr(S, P2, O).

owl-eq-rep-o @
      rdf_chr(O1, 'http://www.w3.org/2002/07/owl#sameAs', O2),
      rdf_chr(S, P, O1)
  ==> mat_deb(owl(eq(rep(s))), [
        rdf(O1, 'http://www.w3.org/2002/07/owl#sameAs', O2),
        rdf(S, P, O1)],
        rdf(S, P, O2))
    | rdf_chr(S, P, O2).

owl-eq-diff1 @
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Y),
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#differentFrom', Y)
  ==> mat_deb(owl(eq(diff1)), [
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Y),
        rdf(X, 'http://www.w3.org/2002/07/owl#differentFrom', Y)],
	error)
    | error(owl-eq-diff1, [
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Y),
        rdf(X, 'http://www.w3.org/2002/07/owl#differentFrom', Y)
      ]).

owl-cax-eqc1 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
      rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1)
  ==> mat_deb(owl(cax(eqc(1))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1)],
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2))
    | rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2).

owl-cax-eqc2 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
      rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2)
  ==> mat_deb(owl(cax(eqc(2))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2)],
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1))
    | rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1).

owl-cls-1 @
      rdf_chr(C, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Class')
  ==> mat_deb(owl(cls(1)), [
        rdf(C, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Class')],
        rdf(C, 'http://www.w3.org/2002/07/owl#equivalentClass', C))
    | rdf_chr(C, 'http://www.w3.org/2002/07/owl#equivalentClass', C).

owl-cls-int-1-1 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L),
      '_allTypes'(L, Y)
  ==> mat_deb(owl(cls(int(1))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L)],
        rdf(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C))
    | rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C).

owl-cls-int-1-2 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY),
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', TL),
      rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY),
      '_allTypes'(TL, Y)
  ==> mat_deb(owl(cls(int(1))), [
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY),
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', TL),
        rdf(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY),
        '_allTypes'(TL, Y)],
        '_allTypes'(L, Y))
    | '_allTypes'(L, Y).

owl-cls-int-1-3 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY),
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
      rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY)
  ==> mat_deb(owl(cls(int(1))), [
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY),
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
        rdf(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY)],
        '_allTypes'(L, Y))
    | '_allTypes'(L, Y).

owl-cls-int-2 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L),
      rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C),
      inList(D, L)
  ==> mat_deb(owl(cls(int(2))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L),
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C),
        inList(D, L)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D).

owl-cls-hv-1 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
      rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C)
  ==> mat_deb(owl(cls(hv(1))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
        rdf(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C)],
        rdf(I, P, V))
    | rdf_chr(I, P, V).

owl-cls-hv-2 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
      rdf_chr(I, P, V)
  ==> mat_deb(owl(cls(hv(2))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
        rdf(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
        rdf(I, P, V)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C).

owl-scm-eqc-1 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)
  ==> mat_deb(owl(scm(eqc(1))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)],
        rdf(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2))
    | rdf_chr(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2).

owl-scm-eqc-11 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)
  ==> mat_deb(owl(scm(eqc(11))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)],
        rdf(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1))
    | rdf_chr(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1).

owl-scm-eqc-2 @
      rdf_chr(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2),
      rdf_chr(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1)
  ==> mat_deb(owl(scm(eqc(2))), [
        rdf(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2),
        rdf(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1)],
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2))
    | rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2).

owl-scm-int @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L),
      inList(D, L)
  ==> mat_deb(owl(scm(int)), [
        rdf(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L),
        inList(D, L)],
        rdf(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D))
    | rdf_chr(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D).

rdf-list-1 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', X)
  ==> mat_deb(rdf(list(1)), [
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', X)],
        inList(X, L))
    | inList(X, L).

rdf-list-2 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', M),
      inList(X, M)
  ==> mat_deb(rdf(list(2)), [
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', M),
        inList(X, M)],
        inList(X, L))
    | inList(X, L).

rdfs-9 @
      rdf_chr(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D),
      rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C)
  ==> mat_deb(rdfs(9), [
        rdf(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D),
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D).
