:- module(
  mat_viz,
  [
    mat_viz/0,
    mat_viz/1  % +Stmt
  ]
).

/** <module> Materialization visualization

Exports materialization results.

@author Wouter Beek
@version 2015/08, 2015/12, 2016/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(solution_sequences)).

:- use_module(library(dcg)).
:- use_module(library(graph/build_export_graph)).
:- use_module(library(gv/gv_file)).
:- use_module(library(hash_ext)).
:- use_module(library(mat/j_db)).
:- use_module(library(mat/mat_print)).
:- use_module(library(os/process_ext)).
:- use_module(library(pl_ext)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/rdf_term)).

:- rdf_meta
   mat_viz(r, r, o).





%! mat_viz is det.
%
% Exports a proof of all deductions.

mat_viz:-
  aggregate_all(
    set(edge(X,Y)),
    ((
      j_db:j(_, _, Y, X)
    ; j_db:j(_, Ps, _, Y),
      member(X, Ps)
    )),
    Es
  ),
  mat_viz0(Es, 'Materialization proof tree').



%! mat_viz(+Stmt) is nondet.
%
% Non-deterministically exports proofs for statements.

mat_viz(Stmt) :-
  md5(Stmt, C),
  % Find all edges.
  findall(E, distinct(E, find_edge(s(C), E)), Es),
  string_phrase(s_label(Stmt), Lbl),
  format(atom(GLbl), "Proof tree for ~a", [Lbl]),
  mat_viz0(Es, GLbl).

mat_viz0(Es, GLabel) :-
  findall(V, distinct(V, (member(edge(X,Y), Es), (V = X ; V = Y))), Vs),
  Opts = [
    graph_directed(true),
    graph_label(GLabel),
    vertex_color(proof_node_color),
    vertex_label(proof_node_label),
    vertex_shape(proof_node_shape)
  ],
  build_export_graph(graph(Vs,Es), ExportG, Opts),
  graph_viz(ExportG, 'j.pdf', [method(dot),output(pdf)]),
  open_file('j.pdf').



%! find_edge(+Node:compound, -Edge:pair(md5)) is nondet.
%
% Returns edges that lead onto Node.
%
% Node is either `j(MD5)` for justifications or `s(MD5)` for statements.

find_edge(X, E) :-
  find_edge(X, [], [], E).

% Edge `P -> J` from premise to justification.
find_edge(j(J), Js, Ss, E) :-
  j_db:j(_, Ps, _, J),
  member(P, Ps),
  maplist(dif(P), Ss),
  (E = edge(P,J) ; find_edge(s(P), Js, [P|Ss], E)).
% Edge `J -> C` from justification to conclusion.
find_edge(s(C), Js, Ss, E) :-
  j_db:j(_, _, C, J),
  maplist(dif(J), Js),
  (E = edge(J,C) ; find_edge(j(J), [J|Js], Ss, E)).


% Statement color.
proof_node_color(X, SColor) :-
  j_db:s(S, X), !,
  (   S == error
  ->  SColor = red
  ;   SColor = blue
  ).
% Justification color.
proof_node_color(X, JColor) :-
  j_db:j(R, _, _, X), !,
  (debugging(mat(R)) -> JColor = red ; JColor = green).


% Statement label.
proof_node_label(X) --> {j_db:s(S, X)}, !, s_label(S).
% Justification label.
proof_node_label(X) --> {j_db:j(R, _, _, X)}, !, print_rule(R).


proof_node_shape(X, SShape) :-
  j_db:s(_, X), !,
  SShape = rect.
proof_node_shape(X, SShape) :-
  j_db:j(_, _, _, X), !,
  SShape = octagon.


s_label(rdf(S,P,O)) --> !,
  rdf_dcg_tp(S, P, O).
s_label(T) -->
   rdf_dcg_node(T).
