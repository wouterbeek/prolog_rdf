:- module(
  j_db,
  [
    prove_all/0,
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

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dif)).
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



%! prove_all is det.
% Exports a proof of all deductions.

prove_all:-
  aggregate_all(
    set(edge(X,Y)),
    ((
      j(_, _, Y, X)
    ; j(_, Ps, _, Y),
      member(X, Ps)
    )),
    Es
  ),
  export_proof_graph(Es, 'Materialization proof tree').

export_proof_graph(Es, GLabel):-
  edges_to_vertices(Es, Vs),
  Opts = [
    graph_directed(true),
    graph_label(GLabel),
    vertex_color(proof_node_color),
    vertex_label(proof_node_label),
    vertex_shape(proof_node_shape)
  ],
  build_export_graph(graph(Vs,Es), ExportG, Opts),
  gv_export(ExportG, 'j.pdf', [method(dot),output(pdf)]),
  handle_process(xpdf, [file('j.pdf')], [program('XPDF')]).

edges_to_vertices(Es, Vs):-
  edges_to_vertices0(Es, Vs0),
  sort(Vs0, Vs).

edges_to_vertices0([], []):- !.
edges_to_vertices0([edge(X,Y)|T1], [X,Y|T2]):-
  edges_to_vertices0(T1, T2).


% Statement color.
proof_node_color(X, Color):-
  s(_, X), !,
  Color = blue.
% Justification color.
proof_node_color(X, Color):-
  j(Rule, _, _, X), !,
  (debugging(mat(Rule)) -> Color = red ; Color = green).

% Statement label.
proof_node_label(X, Label):-
  s(rdf(S,P,O), X), !,
  with_output_to(atom(Label), rdf_print_triple(S, P, O, _, [logic_sym(true)])).
% Justification label.
proof_node_label(X, Label):-
  j(Rule, _, _, X), !,
  owl_mat_deb:rule_label(Rule, Label).

proof_node_shape(X, Shape):-
  s(_, X), !,
  Shape = rect.
proof_node_shape(X, Shape):-
  j(_, _, _, X), !,
  Shape = octagon.



%! prove_triple(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term
%! ) is nondet.
% Exports proofs for the given conclusion.

prove_triple(S, P, O):-
  md5(rdf(S,P,O), C),
  aggregate_all(set(E), find_edge(s(C), E), Es),
  with_output_to(atom(Label), rdf_print_triple(S, P, O, _, [logic_sym(true)])),
  format(atom(GLabel), 'Proof tree for ~a', [Label]),
  export_proof_graph(Es, GLabel).

find_edge(X, E):-
  find_edge(X, [], E).

find_edge(j(J), T, E):-
  j(_, Ps, _, J),
  member(P, Ps),
  maplist(dif(P), T),
  (E = edge(P,J) ; find_edge(s(P), [P|T], E)).
find_edge(s(S), T, E):-
  j(_, _, S, J),
  maplist(dif(J), T),
  (E = edge(J,S) ; find_edge(j(J), [J|T], E)).



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
