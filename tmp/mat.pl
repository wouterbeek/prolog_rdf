:- module(
  mat,
  [
    mat/1,	% +InputG
    mat/2,	% +InputG, ?OutputG
    mat/3	% +InputG, ?OutputG, +Opts
  ]
).

/** <module> OWL materialization

@author Wouter Beek
@version 2015/08, 2015/10, 2015/12-2016/01
*/

:- use_module(library(apply)).
:- use_module(library(chr)).
:- use_module(library(default)).
:- use_module(library(error)).

:- use_module(library(debug_ext)).
:- use_module(library(mat/j_db)).
:- use_module(library(mat/mat_deb)).
:- use_module(library(option_ext)).
:- use_module(library(sw/rdf_term)).

:- rdf_meta
   mat(r),
   mat(r, r),
   mat(r, r, r).

:- set_prolog_flag(chr_toplevel_show_store, false).

:- chr_constraint
   '_allTypes'/2,
   error/0,
   inList/2,
   rdf_chr/3.





%! mat(+InputG) is det.
%
% Wrapper around mat/2 that asserts materialization results into the
% input graph.

mat(G) :-
  mat(G, G).


%! mat(+InputG, +OutputG) is det.
%
% Wrapper around mat/3 with default options.

mat(InG, OutG) :-
  mat(InG, OutG, []).


%! mat(+InputG, +OutputG, +Opts) is det.
%
% Materializes the contents of InputGraph into OutputGraph.

%! mat(+InputG, -OutputG, +Optns) is det.
%
% Materializes the contents of InputGraph into the default graph
% (called `default`).

%! mat(-InputG, +OutputG, +Opts) is det.
%
% Materializes all contents into OutputGraph.

%! mat(-InputG, -OutputG, +Opts) is det.
%
% Materializes all contents into the default graph (called `default`).
%
% The following options are supported:
%
%   * justifications(+boolean)
%
%     Whether justifications for deductions are stored.  Default is
%     `false`.

mat(GIn, GOut, Opts) :-
  call_default_value(GOut, rdf_default_graph),
  (   var(GIn)
  ->  mat0(GIn, GOut, Opts)
  ;   must_be(atom, GIn),
      (rdf_graph(GIn) -> true ; existence_error(rdf_graph, GIn)),

      % Debug message before.
      debug(mat(_), 'BEFORE MATERIALIZATION:', []),
      if_debug(mat(_), rdf_print_triples(_, _, _, GIn)),

      once(mat0(GIn, GOut, Opts)),

      % Debug message for successful materialization results.
      debug(mat(_), 'AFTER MATERIALIZATION:', []),
      if_debug(mat(_), rdf_print_triples(_, _, _, GIn))
  ).

mat0(GIn, GOut, Opts) :-
  % Perform materialization.
  findall(rdf_chr(S,P,O), rdf(S, P, O, GIn), Ins),

  % Store justifications.
  forall(member(rdf_chr(S,P,O), Ins), store_j(axiom, [], rdf(S,P,O))),
  
  maplist(call, Ins),

  % Check whether an inconsistent state was reached.
  (   find_chr_constraint(error(_))
  ->  debug(mat(_), 'INCONSISTENT STATE', []),
      forall(print_j(_,_,error(_),_), true)
  ;   true
  ),

  forall(find_chr_constraint(rdf_chr(S,P,O)), qb(trp, S, P, O, GOut)),

  if_option(justifications(true), Opts, clear_j).





% RULES %

idempotence @
      rdf_chr(S, P, O)
  \   rdf_chr(S, P, O)
  <=> debug(db(idempotence), "Idempotence: ~w", [rdf(S,P,O)])
    | true.

owl-eq-diff1 @
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Y),
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#differentFrom', Y)
  ==> mat_deb(owl(eq(diff1)), [
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Y),
        rdf(X, 'http://www.w3.org/2002/07/owl#differentFrom', Y)],
        error)
    | error.

/*
% Reflexivity of identity for subject terms.
owl-eq-ref @
      rdf_chr(S, P, O)
  ==> mat_deb(owl(eq(ref)), [
        rdf(S, P, O)],
        rdf(S, 'http://www.w3.org/2002/07/owl#sameAs', S))
    | rdf_chr(S, 'http://www.w3.org/2002/07/owl#sameAs', S).

% Reflexivity of identity for predicate terms.
owl-eq-ref1 @
      rdf_chr(S, P, O)
  ==> mat_deb(owl(eq(ref1)), [
        rdf(S, P, O)],
        rdf(P, 'http://www.w3.org/2002/07/owl#sameAs', P))
    | rdf_chr(P, 'http://www.w3.org/2002/07/owl#sameAs', P).

% Reflexivity of identity for object terms.
owl-eq-ref2 @
      rdf_chr(S, P, O)
  ==> mat_deb(owl(eq(ref2)), [
        rdf(S, P, O)],
        rdf(O, 'http://www.w3.org/2002/07/owl#sameAs', O))
    | rdf_chr(O, 'http://www.w3.org/2002/07/owl#sameAs', O).

% Symmetry of identity.
owl-eq-sym @
      rdf_chr(S, 'http://www.w3.org/2002/07/owl#sameAs', O)
  ==> mat_deb(owl(eq(sym)), [
        rdf(S, 'http://www.w3.org/2002/07/owl#sameAs', O)],
        rdf(O, 'http://www.w3.org/2002/07/owl#sameAs', S))
    | rdf_chr(O, 'http://www.w3.org/2002/07/owl#sameAs', S).

% Transitivity of identity.
owl-eq-trans @
      rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Y),
      rdf_chr(Y, 'http://www.w3.org/2002/07/owl#sameAs', Z)
  ==> mat_deb(owl(eq(trans)), [
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Y),
        rdf(Y, 'http://www.w3.org/2002/07/owl#sameAs', Z)],
        rdf(X, 'http://www.w3.org/2002/07/owl#sameAs', Z))
    | rdf_chr(X, 'http://www.w3.org/2002/07/owl#sameAs', Z).

% Replace identical subject term.
owl-eq-rep-s @
      rdf_chr(S1, 'http://www.w3.org/2002/07/owl#sameAs', S2),
      rdf_chr(S1, P, O)
  ==> mat_deb(owl(eq(rep(s))), [
        rdf(S1, 'http://www.w3.org/2002/07/owl#sameAs', S2),
        rdf(S1, P, O)],
        rdf(S2, P, O))
    | rdf_chr(S2, P, O).

% Replace identical predicate term.
owl-eq-rep-p @
      rdf_chr(P1, 'http://www.w3.org/2002/07/owl#sameAs', P2),
      rdf_chr(S, P1, O)
  ==> mat_deb(owl(eq(rep(s))), [
        rdf(P1, 'http://www.w3.org/2002/07/owl#sameAs', P2),
        rdf(S, P1, O)],
        rdf(S, P2, O))
    | rdf_chr(S, P2, O).

% Replace identical object term.
owl-eq-rep-o @
      rdf_chr(O1, 'http://www.w3.org/2002/07/owl#sameAs', O2),
      rdf_chr(S, P, O1)
  ==> mat_deb(owl(eq(rep(s))), [
        rdf(O1, 'http://www.w3.org/2002/07/owl#sameAs', O2),
        rdf(S, P, O1)],
        rdf(S, P, O2))
    | rdf_chr(S, P, O2).
*/

% RDFS label is an OWL annotation property.
owl-prp-ap-label @
      true
  ==> mat_deb(owl(prp(ap(label))), [],
        rdf('http://www.w3.org/2000/01/rdf-schema#label', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2000/01/rdf-schema#label', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

% RDFS comment is an OWL annotation property.
owl-prp-ap-comment @
      true
  ==> mat_deb(owl(prp(ap(comment))), [],
        rdf('http://www.w3.org/2000/01/rdf-schema#comment', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2000/01/rdf-schema#comment', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

% RDFS seeAlso is an OWL annotation property.
owl-prp-ap-seeAlso @
      true
  ==> mat_deb(owl(prp(ap(seeAlso))), [],
        rdf('http://www.w3.org/2000/01/rdf-schema#seeAlso', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2000/01/rdf-schema#seeAlso', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

% OWL deprecated is an OWL annotation property.
owl-prp-ap-deprecated @
      true
  ==> mat_deb(owl(prp(ap(deprecated))), [],
        rdf('http://www.w3.org/2002/07/owl#deprecated', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2002/07/owl#deprecated', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

% OWL prior version is an OWL annotation property.
owl-prp-ap-priorVersion @
      true
  ==> mat_deb(owl(prp(ap(priorVersion))), [],
        rdf('http://www.w3.org/2002/07/owl#priorVersion', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2002/07/owl#priorVersion', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

% OWL backward compatible with is an OWL annotation property.
owl-prp-ap-backwardCompatibleWith @
      true
  ==> mat_deb(owl(prp(ap(backwardCompatibleWith))), [],
        rdf('http://www.w3.org/2002/07/owl#backwardCompatibleWith', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2002/07/owl#backwardCompatibleWith', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

% OWL incompatible with is an OWL annotation property.
owl-prp-ap-incompatibleWith @
      true
  ==> mat_deb(owl(prp(ap(incompatibleWith))), [],
        rdf('http://www.w3.org/2002/07/owl#incompatibleWith', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H))
    | rdf_chr('http://www.w3.org/2002/07/owl#incompatibleWith', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AnnotationProperty', H-H).

% Domain inference.
owl-prp-dom @
      rdf_chr(P, 'http://www.w3.org/2000/01/rdf-schema#domain', C),
      rdf_chr(I, P, O)
  ==> mat_deb(owl(prp(dom)), [
        rdf(P, 'http://www.w3.org/2000/01/rdf-schema#domain', C),
        rdf(I, P, O)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C).

% Functional image.
owl-prp-fp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#FunctionalProperty'),
      rdf_chr(X, P, Y1),
      rdf_chr(X, P, Y2)
  ==> mat_deb(owl(prp(fp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#FunctionalProperty'),
        rdf(X, P, Y1),
        rdf(X, P, Y2)],
        rdf(Y1, 'http://www.w3.org/2002/07/owl#sameAs', Y2))
    | rdf_chr(Y1, 'http://www.w3.org/2002/07/owl#sameAs', Y2).

% Inverse-functional image.
owl-prp-ifp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#InverseFunctionalProperty'),
      rdf_chr(X1, P, Y),
      rdf_chr(X2, P, Y)
  ==> mat_deb(owl(prp(ifp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#InverseFunctionalProperty'),
        rdf(X1, P, Y),
        rdf(X2, P, Y)],
        rdf(X1, 'http://www.w3.org/2002/07/owl#sameAs', X2))
    | rdf_chr(X1, 'http://www.w3.org/2002/07/owl#sameAs', X2).

% Range inference.
owl-prp-rng @
      rdf_chr(P, 'http://www.w3.org/2000/01/rdf-schema#range', C),
      rdf_chr(S, P, I)
  ==> mat_deb(owl(prp(dom)), [
        rdf(P, 'http://www.w3.org/2000/01/rdf-schema#range', C),
        rdf(S, P, I)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C))
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C).

% OWL symmetry.
owl-prp-symp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#SymmetricProperty'),
      rdf_chr(S, P, O)
  ==> mat_deb(owl(prp(symp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#SymmetricProperty'),
        rdf(S, P, O)],
        rdf(O, P, S))
    | rdf_chr(O, P, S).

% OWL asymmetry.
owl-prp-asyp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AsymmetricProperty'),
      rdf_chr(S, P, O),
      rdf_chr(O, P, S)
  ==> mat_deb(owl(prp(symp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#AsymmetricProperty'),
        rdf(S, P, O),
        rdf(O, P, S)],
        error)
    | error.

% OWL transitivity.
owl-prp-trp @
      rdf_chr(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#TransitiveProperty'),
      rdf_chr(X, P, Y),
      rdf_chr(Y, P, Z)
  ==> mat_deb(owl(prp(trp)), [
        rdf(P, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#TransitiveProperty'),
        rdf(X, P, Y),
        rdf(Y, P, Z)],
        rdf(X, P, Z))
    | rdf_chr(X, P, Z).

owl-prp-spo1 @
      rdf_chr(P1, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', P2),
      rdf_chr(S, P1, O)
  ==> mat_deb(owl(prp(spo1)), [
        rdf(P1, 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf', P2),
        rdf(S, P1, O)],
        rdf(S, P2, O))
    | rdf_chr(S, P2, O).

owl-prp-eqp1 @
      rdf_chr(P1, 'http://www.w3.org/2002/07/owl#equivalentProperty', P2),
      rdf_chr(S, P1, O)
  ==> mat_deb(owl(prp(eqp(1))), [
        rdf(P1, 'http://www.w3.org/2002/07/owl#equivalentProperty', P2),
        rdf(S, P1, O)],
        rdf(S, P2, O))
    | rdf_chr(S, P2, O).

owl-prp-eqp2 @
      rdf_chr(P1, 'http://www.w3.org/2002/07/owl#equivalentProperty', P2),
      rdf_chr(S, P2, O)
  ==> mat_deb(owl(prp(eqp(2))), [
        rdf(P1, 'http://www.w3.org/2002/07/owl#equivalentProperty', P2),
        rdf(S, P2, O)],
        rdf(S, P1, O))
    | rdf_chr(S, P1, O).

owl-prp-pdw @
      rdf_chr(P1, 'http://www.w3.org/2002/07/owl#propertyDisjointWith', P2),
      rdf_chr(S, P1, O),
      rdf_chr(S, P2, O)
  ==> mat_deb(owl(prp(pdw)), [
        rdf(P1, 'http://www.w3.org/2002/07/owl#propertyDisjointWith', P2),
        rdf(S, P1, O),
        rdf(S, P2, O)],
        error)
    | error.

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
        rdf(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L),
        '_allTypes'(L, Y)],
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
