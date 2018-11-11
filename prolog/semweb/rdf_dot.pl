:- module(
  rdf_dot,
  [
    rdf_dot_arc/3,         % +Out, +FromNode, +ToNode
    rdf_dot_arc/4,         % +Out, +FromNode, +ToNode, +Options
    rdf_dot_cluster/3,     % +Out, +Node, :Goal_1
    rdf_dot_cluster/4,     % +Out, +Node, :Goal_1, +Options
    rdf_dot_cluster_arc/3, % +Out, +FromNode, +ToNode
    rdf_dot_cluster_arc/4, % +Out, +FromNode, +ToNode, +Options
    rdf_dot_node/2,        % +Out, +Node
    rdf_dot_node/3,        % +Out, +Node, +Options
    rdf_dot_node_uml/3,    % +Out, +Backend, +Node
    rdf_dot_node_uml/4     % +Out, +Backend, +Node, +Options
  ]
).
:- reexport(library(graph/dot)).

/** <module> RDF DOT

Extension of library(graph/dot) for exporting RDF nodes and arcs.

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(yall)).

:- use_module(library(dcg)).
:- use_module(library(default)).
:- use_module(library(graph/dot)).
:- use_module(library(semweb/rdf_api)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_print)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(string_ext)).

:- meta_predicate
    rdf_dot_cluster(+, +, 1),
    rdf_dot_cluster(+, +, 1, +).

:- rdf_meta
   rdf_dot_arc(+, r, r),
   rdf_dot_arc(+, r, r, +),
   rdf_dot_cluster(+, r, :),
   rdf_dot_cluster(+, r, :, +),
   rdf_dot_cluster_arc(+, r, r),
   rdf_dot_cluster_arc(+, r, r, +),
   rdf_dot_node(+, r),
   rdf_dot_node(+, r, +),
   rdf_dot_node_uml(+, t, r),
   rdf_dot_node_uml(+, t, r, +).





%! rdf_dot_arc(+Out:stream, +FromNode:rdf_node, +ToNode:rdf_node) is det.
%! rdf_dot_arc(+Out:stream, +FromNode:rdf_node, +ToNode:rdf_node, +Options:dict) is det.

rdf_dot_arc(Out, FromNode, ToNode) :-
  rdf_dot_arc(Out, FromNode, ToNode, options{}).


rdf_dot_arc(Out, FromNode, ToNode, Options) :-
  dot_arc(Out, FromNode, ToNode, Options).



%! rdf_dot_cluster(+Out:stream, +Node:rdf_node, :Goal_1) is det.
%! rdf_dot_cluster(+Out:stream, +Node:rdf_node, :Goal_1, +Options:dict) is det.

rdf_dot_cluster(Out, Node, Goal_1) :-
  rdf_dot_cluster(Out, Node, Goal_1, options{}).


rdf_dot_cluster(Out, Node, Goal_1, Options) :-
  dot_cluster(Out, Node, Goal_1, Options).



%! rdf_dot_cluster_arc(+Out:stream, +FromNode:rdf_node, +ToNode:rdf_node) is det.
%! rdf_dot_cluster_arc(+Out:stream, +FromNode:rdf_node, +ToNode:rdf_node, +Options:dict) is det.

rdf_dot_cluster_arc(Out, FromNode, ToNode) :-
  rdf_dot_cluster_arc(Out, FromNode, ToNode, options{}).


rdf_dot_cluster_arc(Out, FromNode, ToNode, Options) :-
  dot_cluster_arc(Out, FromNode, ToNode, Options).



%! rdf_dot_node(+Out:stream, +Node:rdf_node) is det.
%! rdf_dot_node(+Out:stream, +Node:rdf_node, +Options:dict) is det.

rdf_dot_node(Out, Node) :-
  rdf_dot_node(Out, Node, options{}).


rdf_dot_node(Out, Node, Options) :-
  string_phrase(rdf_dcg_node(Node, Options), Label),
  dot_node(Out, Node, _{label: Label}).



%! rdf_dot_node_uml(+Out:stream, +Backend, +Node:rdf_node) is det.
%! rdf_dot_node_uml(+Out:stream, +Backend, +Node:rdf_node, +Options:dict) is det.

rdf_dot_node_uml(Out, B, Node) :-
  rdf_dot_node_uml(Out, B, Node, options{}).


rdf_dot_node_uml(Out, B, Node, Options0) :-
  string_phrase(rdf_dcg_node(Node, Options0), NodeString),
  H = [cell(colspan(2), b(NodeString))],
  % literals
  findall(
    [cell(PString),cell(OString)],
    (
      triple(B, Node, P, O),
      rdf_is_literal(O),
      string_phrase(rdf_dcg_predicate(P, Options0), PString),
      string_phrase(rdf_dcg_node(O, Options0), OString)
    ),
    T1
  ),
  % RDF lists
  findall(
    [cell(PString),cell(table(border(0),[Row]))],
    (
      list_triple(B, Node, P, L),
      string_phrase(rdf_dcg_predicate(P), PString),
      maplist(rdf_dot_node_cell_, L, Row)
    ),
    T2
  ),
  append(T1, T2, T),
  merge_options(Options0, options{html: table([H|T])}, Options),
  dot_node(Out, Node, Options).

rdf_dot_node_cell_(Term, cell(String)) :-
  string_phrase(rdf_dcg_node(Term), String).
