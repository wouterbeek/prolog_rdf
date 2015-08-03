:- module(
  j_db,
  [
    prove_triple/3, % +Subject:or([bnode,iri])
                    % +Predicate:iri
                    % +Object:rdf_term
    store_j/3 % +Rule:compound
              % +Predicates:list(compound)
              % +Conclusion:compound
  ]
).

/** <module> Justification database

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(graph/build_export_graph)).
:- use_module(library(gv/gv_file)).
:- use_module(library(hash_ext)).
:- use_module(library(lists)).
:- use_module(library(process_ext)).

:- rdf_meta(prove_triple(r,r,o)).

%! j(
%!   ?Rule:atom,
%!   ?Predicate:list(atom),
%!   ?Conclusion:atom,
%!   ?Hash:atom
%! ) is nondet.

:- dynamic(j/4).

%! s(?Triple:compound, ?Hash:atom) is nondet.

:- dynamic(s/2).



prove_triple(S, P, O):-
  md5(rdf(S,P,O), HC),
  j(_,_,HC,HJ),
  prove_j(HJ).

prove_j(J):-
  collect_edges(J, Es),
  edges_to_vertices(Es, Vs),
  build_export_graph(graph(Vs,[],Es,[]), ExportG, []),
  gv_export(ExportG, 'j.pdf', [method(dot),output(pdf)]),
  handle_process(xpdf, [file('j.pdf')], [program('XPDF')]).

collect_edges(J, Es):-
  collect_edges(J, [], Es0),
  sort(Es0, Es).

collect_edges(J1, Es0, Es):-
  j(_, Ps1, _, J1),
  member(C2, Ps1),
  j(_, _, C2, J2),
  collect_edges(J2, [edge(J1,J2)|Es0], Es).
collect_edges(_, Es, Es).

edges_to_vertices(Es, Vs):-
  edges_to_vertices0(Es, Vs0),
  sort(Vs0, Vs).

edges_to_vertices0([], []):- !.
edges_to_vertices0([edge(X,Y)|T1], [X,Y|T2]):-
  edges_to_vertices0(T1, [T2]).



%! store_j(
%!   +Rule:compound,
%!   +Predicates:list(compound),
%!   +Conclusion:compound
%! ) is det.

store_j(Rule, Ps, C):-
  maplist(store_s, [C|Ps], [HC|HPs]),
  md5(j(Rule, HPs, HC), HJ),
  store_j0(Rule, HPs, HC, HJ).

store_j0(_, _, _, HJ):-
  j(_, _, _, HJ), !.
store_j0(Rule, HPs, HC, HJ):-
  assert(j(Rule,HPs,HC,HJ)).

store_s(T, H):-
  md5(T, H),
  store_s0(T, H).

store_s0(T, H):-
  s(T, H), !.
store_s0(T, H):-
  assert(s(T,H)).
