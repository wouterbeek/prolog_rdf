:- module(
  mat,
  [
    mat/0,
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
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(mat/j_db)).
:- use_module(library(mat/mat_deb)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_graph)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(rdf/rdf_read)).

:- set_prolog_flag(chr_toplevel_show_store, false).

:- chr_constraint('_allTypes'/3).
:- chr_constraint(error/1).
:- chr_constraint(inList/3).
:- chr_constraint(rdf_chr/4).





%! mat is det.
% Wrapper around mat(-,-).

mat:-
  mat(_, _).

%! mat(+InputGraph:atom) is det.
% Wrapper around mat(+,-).

mat(GIn):-
  mat(GIn, _).

%! mat(+InputGraph:atom, +OutputGraph:atom) is det.
% Materializes the contents of InputGraph into OutputGraph.
%! mat(+InputGraph:atom, -OutputGraph:atom) is det.
% Materializes the contents of InputGraph into the default graph
% (called `user`).
%! mat(-InputGraph:atom, +OutputGraph:atom) is det.
% Materializes all contents into OutputGraph.
%! mat(-InputGraph:atom, -OutputGraph:atom) is det.
% Materializes all contents into the default graph (called `user`).

mat(GIn, GOut):-
  defval(user, GOut),
  (   var(GIn)
  ->  mat0(GIn, GOut)
  ;   must_be(atom, GIn),
      (   rdf_is_graph(GIn)
      ->  true
      ;   existence_error(rdf_graph, GIn)
      ),

      % Debug message before.
      PrintOpts = [abbr_list(true),indent(1)],
      debug(mat(_), 'BEFORE MATERIALIZATION:', []),
      if_debug(mat(_), rdf_print_graph(GIn, PrintOpts)),

      once(mat0(GIn, GOut)),

      % Debug message for successful materialization results.
      debug(mat(_), 'AFTER MATERIALIZATION:', []),
      if_debug(mat(_), rdf_print_graph(GIn, PrintOpts))
  ).

mat0(GIn, GOut):-
  % Perform materialization.
  findall(rdf_chr(S,P,O,[GIn]), rdf2(S,P,O,GIn), Ins),
  forall(member(rdf_chr(S,P,O,Gs), Ins), store_j(axiom, [], rdf(S,P,O,Gs))),
  maplist(call, Ins),

  % Check whether an inconsistent state was reached.
  (   find_chr_constraint(error(_))
  ->  debug(mat(_), 'INCONSISTENT STATE', []),
      forall(print_j(_,_,error(_),_), true)
  ;   true
  ),
  forall(
    find_chr_constraint(rdf_chr(S,P,O,Gs)),
    rdf_assert_in_graph(S, P, O, Gs, GOut)
  ).

% Assert into a named graph.
rdf_assert_in_graph(S, P, O, [G|Gs], _):-
  maplist(=(G), Gs), !,
  rdf_assert2(S, P, O, G).
% Assert into the default graph.
rdf_assert_in_graph(S, P, O, _, GOut):-
  rdf_assert2(S, P, O, GOut).





% RULES %

idempotence @
      rdf_chr(S, P, O, Gs)
  \   rdf_chr(S, P, O, Gs)
  <=> debug(db(idempotence), 'idempotence', rdf(S,P,O,Gs))
    | true.

owl-eq-diff1 @
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Y, Gs1),
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#differentFrom', Y, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(eq(diff1)), [
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Y, Gs1),
        rdf(X, 'http://www.w3.org/2002/07/owl#differentFrom', Y, Gs2)],
        error(Gs))
    | error(Gs).

owl-eq-ref @
      rdf_chr(S, P, O, Gs)
  ==> mat_deb(owl(eq(ref)), [
        rdf(S, P, O, Gs)],
        rdf(S, 'http://www.w3.org/2002/07/owl#sameAs', S, Gs))
    | rdf_chr(S, 'http://www.w3.org/2002/07/owl#sameAs', S, Gs).

owl-eq-ref1 @
      rdf_chr(S, P, O, Gs)
  ==> mat_deb(owl(eq(ref1)), [
        rdf(S, P, O, Gs)],
        rdf(P, 'http://www.w3.org/2002/07/owl#sameAs', P, Gs))
    | rdf_chr(P, 'http://www.w3.org/2002/07/owl#sameAs', P, Gs).

owl-eq-ref2 @
      rdf_chr(S, P, O, Gs)
  ==> mat_deb(owl(eq(ref2)), [
        rdf(S, P, O, Gs)],
        rdf(O, 'http://www.w3.org/2002/07/owl#sameAs', O, Gs))
    | rdf_chr(O, 'http://www.w3.org/2002/07/owl#sameAs', O, Gs).

owl-eq-sym @
      rdf_chr(S, 'http://www.w3.org/2002/07/owl#sameAs', O, Gs)
  ==> mat_deb(owl(eq(sym)), [
        rdf(S, 'http://www.w3.org/2002/07/owl#sameAs', O, Gs)],
        rdf(O, 'http://www.w3.org/2002/07/owl#sameAs', S, Gs))
    | rdf_chr(O, 'http://www.w3.org/2002/07/owl#sameAs', S, Gs).

owl-eq-trans @
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Y, Gs1),
      rdf_chr(Y, 'http://www.w3.org/2002/07/owl#sameAs', Z, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(eq(trans)), [
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Y, Gs1),
        rdf(Y, 'http://www.w3.org/2002/07/owl#sameAs', Z, Gs2)],
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Z, Gs))
    | rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Z, Gs).

owl-eq-rep-s @
      rdf_chr(S1, 'http://www.w3.org/2002/07/owl#sameAs', S2, Gs1),
      rdf_chr(S1, P, O, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(eq(rep(s))), [
        rdf(S1, 'http://www.w3.org/2002/07/owl#sameAs', S2, Gs1),
        rdf(S1, P, O, Gs2)],
        rdf(S2, P, O, Gs))
    | rdf_chr(S2, P, O, Gs).

owl-eq-rep-p @
      rdf_chr(P1, 'http://www.w3.org/2002/07/owl#sameAs', P2, Gs1),
      rdf_chr(S, P1, O, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(eq(rep(s))), [
        rdf(P1, 'http://www.w3.org/2002/07/owl#sameAs', P2, Gs1),
        rdf(S, P1, O, Gs2)],
        rdf(S, P2, O, Gs))
    | rdf_chr(S, P2, O, Gs).

owl-eq-rep-o @
      rdf_chr(O1, 'http://www.w3.org/2002/07/owl#sameAs', O2, Gs1),
      rdf_chr(S, P, O1, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(eq(rep(s))), [
        rdf(O1, 'http://www.w3.org/2002/07/owl#sameAs', O2, Gs1),
        rdf(S, P, O1, Gs2)],
        rdf(S, P, O2, Gs))
    | rdf_chr(S, P, O2, Gs).

owl-eq-diff1 @
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Y, Gs1),
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#differentFrom', Y, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(eq(diff1)), [
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Y, Gs1),
        rdf(X, 'http://www.w3.org/2002/07/owl#differentFrom', Y, Gs2)],
	error(Gs))
    | error(Gs).

owl-prp-ap-label @
      true
  ==> mat_deb(owl(prp(ap(label))), [],
        rdf('http://www.w3.org/2000/01/rdf-schema#label', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2000/01/rdf-schema#label', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

owl-prp-ap-comment @
      true
  ==> mat_deb(owl(prp(ap(comment))), [],
        rdf('http://www.w3.org/2000/01/rdf-schema#comment', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2000/01/rdf-schema#comment', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

owl-prp-ap-seeAlso @
      true
  ==> mat_deb(owl(prp(ap(seeAlso))), [],
        rdf('http://www.w3.org/2000/01/rdf-schema#seeAlso', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2000/01/rdf-schema#seeAlso', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

owl-prp-ap-deprecated @
      true
  ==> mat_deb(owl(prp(ap(deprecated))), [],
        rdf('http://www.w3.org/2002/07/owl#deprecated', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2002/07/owl#deprecated', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

owl-prp-ap-priorVersion @
      true
  ==> mat_deb(owl(prp(ap(priorVersion))), [],
        rdf('http://www.w3.org/2002/07/owl#priorVersion', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2002/07/owl#priorVersion', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

owl-prp-ap-backwardCompatibleWith @
      true
  ==> mat_deb(owl(prp(ap(backwardCompatibleWith))), [],
        rdf('http://www.w3.org/2002/07/owl#backwardCompatibleWith', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2002/07/owl#backwardCompatibleWith', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

owl-prp-ap-incompatibleWith @
      true
  ==> mat_deb(owl(prp(ap(incompatibleWith))), [],
        rdf('http://www.w3.org/2002/07/owl#incompatibleWith', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2002/07/owl#incompatibleWith', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

owl-prp-dom @
      rdf_chr(P, 'http://www.w3.org/2000/01/rdf-schema#domain', C, Gs1),
      rdf_chr(I, P, O, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(prp(dom)), [
        rdf(P, 'http://www.w3.org/2000/01/rdf-schema#domain', C, Gs1),
        rdf(I, P, O, Gs2)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs).

owl-prp-fp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#FunctionalProperty', Gs1),
      rdf_chr(X, P, Y1, Gs2),
      rdf_chr(X, P, Y2, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(prp(fp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#FunctionalProperty', Gs1),
        rdf(X, P, Y1, Gs2),
        rdf(X, P, Y2, Gs3)],
        rdf(Y1, 'http://www.w3.org/2002/07/owl#sameAs', Y2, Gs))
    | rdf_chr(Y1, 'http://www.w3.org/2002/07/owl#sameAs', Y2, Gs).

owl-prp-rng @
      rdf_chr(P, 'http://www.w3.org/2000/01/rdf-schema#range', C, Gs1),
      rdf_chr(S, P, I, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(prp(dom)), [
        rdf(P, 'http://www.w3.org/2000/01/rdf-schema#range', C, Gs1),
        rdf(S, P, I, Gs2)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs).

owl-prp-fp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#FunctionalProperty', Gs1),
      rdf_chr(X, P, Y1, Gs2),
      rdf_chr(X, P, Y2, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(prp(fp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#FunctionalProperty', Gs1),
        rdf(X, P, Y1, Gs2),
        rdf(X, P, Y2, Gs3)],
        rdf(Y1, 'http://www.w3.org/2002/07/owl#sameAs', Y2, Gs))
    | rdf_chr(Y1, 'http://www.w3.org/2002/07/owl#sameAs', Y2, Gs).

owl-prp-ifp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#InverseFunctionalProperty', Gs1),
      rdf_chr(X1, P, Y, Gs2),
      rdf_chr(X2, P, Y, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(prp(ifp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#InverseFunctionalProperty', Gs1),
        rdf(X1, P, Y, Gs2),
        rdf(X2, P, Y, Gs3)],
        rdf(X1, 'http://www.w3.org/2002/07/owl#sameAs', X2, Gs))
    | rdf_chr(X1, 'http://www.w3.org/2002/07/owl#sameAs', X2, Gs).

owl-prp-symp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#SymmetricProperty', Gs1),
      rdf_chr(S, P, O, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(prp(symp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#SymmetricProperty', Gs1),
        rdf(S, P, O, Gs2)],
        rdf(O, P, S, Gs))
    | rdf_chr(O, P, S, Gs).

owl-prp-asyp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AsymmetricProperty', Gs1),
      rdf_chr(S, P, O, Gs2),
      rdf_chr(O, P, S, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(prp(symp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AsymmetricProperty', Gs1),
        rdf(S, P, O, Gs2),
        rdf(O, P, S, Gs3)],
        error(Gs))
    | error(Gs).

owl-prp-trp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#TransitiveProperty', Gs1),
      rdf_chr(X, P, Y, Gs2),
      rdf_chr(Y, P, Z, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(prp(trp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#TransitiveProperty', Gs1),
        rdf(X, P, Y, Gs2),
        rdf(Y, P, Z, Gs3)],
        rdf(X, P, Z, Gs))
    | rdf_chr(X, P, Z, Gs).

owl-prp-spo1 @
      rdf_chr(P1, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', P2, Gs1),
      rdf_chr(S, P1, O, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(prp(spo1)), [
        rdf(P1, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', P2, Gs1),
        rdf(S, P1, O, Gs2)],
        rdf(S, P2, O, Gs))
    | rdf_chr(S, P2, O, Gs).

owl-prp-eqp1 @
      rdf_chr(P1, 'http://www.w3.org/2002/07/owl#equivalentProperty', P2, Gs1),
      rdf_chr(S, P1, O, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(prp(eqp(1))), [
        rdf(P1, 'http://www.w3.org/2002/07/owl#equivalentProperty', P2, Gs1),
        rdf(S, P1, O, Gs2)],
        rdf(S, P2, O, Gs))
    | rdf_chr(S, P2, O, Gs).

owl-prp-eqp2 @
      rdf_chr(P1, 'http://www.w3.org/2002/07/owl#equivalentProperty', P2, Gs1),
      rdf_chr(S, P2, O, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(prp(eqp(2))), [
        rdf(P1, 'http://www.w3.org/2002/07/owl#equivalentProperty', P2, Gs1),
        rdf(S, P2, O, Gs2)],
        rdf(S, P1, O, Gs))
    | rdf_chr(S, P1, O, Gs).

owl-prp-pdw @
      rdf_chr(P1, 'http://www.w3.org/2002/07/owl#propertyDisjointWith', P2, Gs1),
      rdf_chr(S, P1, O, Gs2),
      rdf_chr(S, P2, O, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(prp(pdw)), [
        rdf(P1, 'http://www.w3.org/2002/07/owl#propertyDisjointWith', P2, Gs1),
        rdf(S, P1, O, Gs2),
        rdf(S, P2, O, Gs3)],
        error(Gs))
    | error(Gs).

owl-cax-eqc1 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs1),
      rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(cax(eqc(1))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs1),
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1, Gs2)],
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2, Gs))
    | rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2, Gs).

owl-cax-eqc2 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs1),
      rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(cax(eqc(2))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs1),
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2, Gs2)],
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1, Gs))
    | rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1, Gs).

owl-cls-1 @
      rdf_chr(C, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Class', Gs)
  ==> mat_deb(owl(cls(1)), [
        rdf(C, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Class', Gs)],
        rdf(C, 'http://www.w3.org/2002/07/owl#equivalentClass', C, Gs))
    | rdf_chr(C, 'http://www.w3.org/2002/07/owl#equivalentClass', C, Gs).

owl-cls-int-1-1 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L, Gs1),
      '_allTypes'(L, Y, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(cls(int(1))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L, Gs1),
        '_allTypes'(L, Y, Gs2)],
        rdf(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs1))
    | rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs).

owl-cls-int-1-2 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY, Gs1),
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', TL, Gs2),
      rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY, Gs3),
      '_allTypes'(TL, Y, Gs4)
  ==> ord_union([Gs1,Gs2,Gs3,Gs4], Gs),
      mat_deb(owl(cls(int(1))), [
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY, Gs1),
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', TL, Gs2),
        rdf(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY, Gs3),
        '_allTypes'(TL, Y, Gs4)],
        '_allTypes'(L, Y, Gs))
    | '_allTypes'(L, Y, Gs).

owl-cls-int-1-3 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY, Gs1),
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil', Gs2),
      rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(cls(int(1))), [
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY, Gs1),
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil', Gs2),
        rdf(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY, Gs3)],
        '_allTypes'(L, Y, Gs))
    | '_allTypes'(L, Y, Gs).

owl-cls-int-2 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L, Gs1),
      rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs2),
      inList(D, L, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(cls(int(2))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L, Gs1),
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs2),
        inList(D, L, Gs3)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D, Gs))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D, Gs).

owl-cls-hv-1 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#hasValue', V, Gs1),
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#onProperty', P, Gs2),
      rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(cls(hv(1))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#hasValue', V, Gs1),
        rdf(C, 'http://www.w3.org/2002/07/owl#onProperty', P, Gs2),
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs3)],
        rdf(I, P, V, Gs))
    | rdf_chr(I, P, V, Gs).

owl-cls-hv-2 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#hasValue', V, Gs1),
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#onProperty', P, Gs2),
      rdf_chr(I, P, V, Gs3)
  ==> ord_union([Gs1,Gs2,Gs3], Gs),
      mat_deb(owl(cls(hv(2))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#hasValue', V, Gs1),
        rdf(C, 'http://www.w3.org/2002/07/owl#onProperty', P, Gs2),
        rdf(I, P, V, Gs3)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs).

owl-scm-eqc-1 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs)
  ==> mat_deb(owl(scm(eqc(1))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs)],
        rdf(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2, Gs))
    | rdf_chr(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2, Gs).

owl-scm-eqc-11 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs)
  ==> mat_deb(owl(scm(eqc(11))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs)],
        rdf(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1, Gs))
    | rdf_chr(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1, Gs).

owl-scm-eqc-2 @
      rdf_chr(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2, Gs1),
      rdf_chr(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(scm(eqc(2))), [
        rdf(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2, Gs1),
        rdf(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1, Gs2)],
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs))
    | rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2, Gs).

owl-scm-int @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L, Gs1),
      inList(D, L, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(owl(scm(int)), [
        rdf(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L, Gs1),
        inList(D, L, Gs2)],
        rdf(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D, Gs))
    | rdf_chr(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D, Gs).

rdf-list-1 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', X, Gs)
  ==> mat_deb(rdf(list(1)), [
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', X, Gs)],
        inList(X, L, Gs))
    | inList(X, L, Gs).

rdf-list-2 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', M, Gs1),
      inList(X, M, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(rdf(list(2)), [
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', M, Gs1),
        inList(X, M, Gs2)],
        inList(X, L, Gs))
    | inList(X, L, Gs).

rdfs-9 @
      rdf_chr(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D, Gs1),
      rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs2)
  ==> ord_union(Gs1, Gs2, Gs),
      mat_deb(rdfs(9), [
        rdf(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D, Gs1),
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C, Gs2)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D, Gs))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D, Gs).
