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
:- use_module(library(debug)).
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
  <=> debug(mat(rdf), idempotence, [])
    | true.

cax-eqc1 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
      rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1)
  ==> debug(mat(owl), 'cax-eqc1', [])
    | rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2).

cax-eqc2 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2),
      rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C2)
  ==> debug(mat(owl), 'cax-eqc2', [])
    | rdf_chr(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C1).

cls1 @
      rdf_chr(C, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.w3.org/2002/07/owl#Class')
  ==> debug(mat(owl), 'cls1', [])
    | rdf_chr(C, 'http://www.w3.org/2002/07/owl#equivalentClass', C).

scm-eqc1 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)
  ==> debug(mat(owl), 'scm-eqc1', [])
    | rdf_chr(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2).

scm-eqc11 @
      rdf_chr(C1, 'http://www.w3.org/2002/07/owl#equivalentClass', C2)
  ==> debug(mat(owl), 'scm-eqc11', [])
    | rdf_chr(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1).

scm-eqc2 @
      rdf_chr(C1, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C2),
      rdf_chr(C2, 'http://www.w3.org/2000/01/rdf-schema#subClassOf', C1)
  ==> debug(mat(owl), 'scm-eqc2', [])
    | rdf_chr(C1, 'http://www.w3.org/2000/01/rdf-schema#equivalentClass', C2).

cls-int1-1 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#intersectionOf', L),
      '_allTypes'(L, Y)
  ==> debug(mat(owl), 'cls-int1-1', [])
    | rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C).

cls-int1-2 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY),
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', TL),
      rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY),
      '_allTypes'(TL, Y)
  ==> debug(mat(owl), 'cls-int1-2', [])
    | '_allTypes'(L, Y).

cls-int1-3 @
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', TY),
      rdf_chr(L, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil'),
      rdf_chr(Y, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', TY)
  ==> debug(mat(owl), 'cls-int1-3', [])
    | '_allTypes'(L, Y).

cls-hv1 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
      rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C)
  ==> debug(mat(owl), 'cls-hv1', [])
    | rdf_chr(I, P, V).

cls-hv2 @
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#hasValue', V),
      rdf_chr(C, 'http://www.w3.org/2002/07/owl#onProperty', P),
      rdf_chr(I, P, V)
  ==> debug(mat(owl), 'cls-hv2', [])
    | rdf_chr(I, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', C).
