:- module(
  owl_mat,
  [
    owl_mat/1, % +InputGraph:atom
    owl_mat/2 % +InputGraph:atom
              % -OutputGraph:atom
  ]
).

/** <module> OWL materialization

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(chr)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_bracketed)).
:- use_module(library(dcg/dcg_logic)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(deb_ext)).
:- use_module(library(error)).
:- use_module(library(rdf/rdf_print)).
:- use_module(library(semweb/rdf_db)).

:- chr_constraint('_allTypes'/2).
:- chr_constraint(rdf_chr/3).



owl_mat(GIn):-
  must_be(atom, GIn),
  (   rdf_graph(GIn)
  ->  true
  ;   existence_error(rdf_graph, GIn)
  ),
  format(user_output, 'BEFORE MATERIALIZATION:\n', []),
  rdf_print_graph(GIn, [indent(1)]),
  owl_mat(GIn, GOut),
  format(user_output, 'AFTER MATERIALIZATION:\n', []),
  rdf_print_graph(GOut, [indent(1)]).


owl_mat(GIn, GOut):-
  findall(rdf_chr(S,P,O), rdf(S,P,O,GIn), Ins),
  maplist(call, Ins),
  atomic_list_concat([GIn,mat], '_', GOut),
  findall(rdf(S,P,O), find_chr_constraint(rdf_chr(S,P,O)), Outs),
  maplist(rdf_assert0(GOut), Outs).

rdf_assert0(G, rdf(S,P,O)):-
  rdf_assert(S, P, O, G).

idempotence @
      rdf_chr(S, P, O)
  \   rdf_chr(S, P, O)
  <=> debug0(db(idempotence), 'Idempotence (~w,~w,~w)', [S,P,O])
    | true.

rdfs-9 @
      rdf_chr(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D),
      rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C)
  ==> debug0(rdfs(9), [
        rdf_chr(C, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', D),
        rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C)],
        rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D))
      | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', D).

owl-cax-eqc1 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
      rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1)
  ==> debug0(owl(cax(eqc(1))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1)],
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2))
      | rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2).

owl-cax-eqc2 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
      rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2)
  ==> debug0(owl(cax(eqc(2))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2)],
        rdf(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1))
      | rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1).

owl-cls-1 @
      rdf_chr(C, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Class')
  ==> debug0(owl(cls(1)), [
        rdf(C, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Class')],
        rdf(C, 'http://www.w3.org/2002/07/owl#equivalentClass', C))
      | rdf_chr(C, 'http://www.w3.org/2002/07/owl#equivalentClass', C).

owl-cls-int-1-1 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L),
      '_allTypes'(L, Y)
  ==> debug0(owl(cls(int(1))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L)],
        rdf(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C))
      | rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C).

owl-cls-int-1-2 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY),
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', TL),
      rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY),
      '_allTypes'(TL, Y)
  ==> debug0(owl(cls(int(1))), [
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
  ==> debug0(owl(cls(int(1))), [
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY),
        rdf(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
        rdf(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY)],
        '_allTypes'(L, Y))
      | '_allTypes'(L, Y).

owl-cls-hv-1 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
      rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C)
  ==> debug0(owl(cls(hv(1))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
        rdf(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C)],
        rdf_chr(I, P, V))
     | rdf_chr(I, P, V).

owl-cls-hv-2 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
      rdf_chr(I, P, V)
  ==> debug0(owl(cls(hv(2))), [
        rdf(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
        rdf(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
        rdf(I, P, V)],
        rdf(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C))
     | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C).

owl-scm-eqc-1 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)
  ==> debug0(owl(scm(eqc(1))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)],
        rdf(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2))
      | rdf_chr(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2).

owl-scm-eqc-11 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)
  ==> debug0(owl(scm(eqc(11))), [
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)],
        rdf(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1))
      | rdf_chr(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1).

owl-scm-eqc-2 @
      rdf_chr(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2),
      rdf_chr(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1)
  ==> debug0(owl(scm(eqc(2))), [
        rdf(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2),
        rdf(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1)],
        rdf(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2))
      | rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2).

debug0(Rule, Ps, C):-
  debugging(mat(Rule)), !,
  rule_label(Rule, Label),
  string_phrase(print_deduction(Label, Ps, C, [logic_sym(true)]), S),
  debug(mat(Rule), '~s', S).
debug0(_, _, _).

rule_label(Rule, Label):-
  unwind_compound(Rule, Label0),
  atomic_list_concat(Label0, ':', Label).

unwind_compound(H, [H]):-
  atomic(H), !.
unwind_compound(H0, [H|T]):-
  H0 =.. [H,T0],
  unwind_compound(T0, T).

print_deduction(Rule, Ps, C, Opts) -->
  bracketed(square, atom(Rule)),
  "\n",
  print_premises(1, Ps, Opts),
  print_conclusion(C, Opts).

print_premises(_, [], _) --> "", !.
print_premises(N1, [H|T], Opts) -->
  "  ",
  print_premise(N1, H, Opts),
  "\n",
  {succ(N1, N2)},
  print_premises(N2, T, Opts).

print_premise(N, rdf(S,P,O), Opts) -->
  "P",
  integer(N),
  ": ",
  rdf_print:rdf_print_statement(S, P, O, _, Opts).

print_conclusion(rdf(S, P, O), Opts) -->
  "  ",
  provable,
  "   ",
  rdf_print:rdf_print_statement(S, P, O, _, Opts).
