:- module(
  rdf_graph,
  [
    rdf_graph_instance/3, % +Instance:atom
                          % +Graph:rdf_graph
                          % -Map:list(pair(bnode,rdf_term))
    rdf_graph_merge/2, % +MergingGraphs:list(atom)
                       % +MergedGraph:rdf_graph
    rdf_graph_same_size/2, % +Graph1:atom
                           % +Graph2:atom
    rdf_is_ground_graph/1, % +Graph:rdf_graph
    rdf_proper_graph_instance/3, % +Graph1:atom
                                 % +Graph2:atom
                                 % -BNodeMap:list(pair(bnode,or([iri,literal])))
    rdf_proper_subgraph/2, % +ProperSubgraph:atom
                           % +Graph:rdf_graph
    rdf_subgraph/2 % +ProperSubgraph:atom
                   % +Graph:rdf_graph
  ]
).

/** <module> RDF graph

@author Wouter Beek
@compat [RDF 1.1 Semantics](http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/)
@version 2012/01-2013/05, 2013/07-2013/08, 2013/11, 2014/04-2014/05, 2014/11,
         2015/03-2015/04, 2015/12
*/

:- use_module(library(error)).
:- use_module(library(ordsets)).
:- use_module(library(q/qb)).
:- use_module(library(q/q_rdf)).





%! rdf_graph_instance(
%!   +Instance:atom,
%!   +Graph:rdf_graph,
%!   -Map:list(pair(bnode,rdf_term))
%! ) is semidet.
% Suppose that M is a functional mapping from a set of blank nodes to
%  some set of literals, blank nodes and IRIs.
% Any graph obtained from a graph G by replacing some or all of
%  the blank nodes N in G by M(N) is an **instance** of G.
%
% Any graph is an instance of itself.
% An instance of an instance of G is an instance of G.
% If H is an instance of G then every triple in H is an instance of
%  at least one triple in G.

rdf_graph_instance(M, H, G, Map) :-
  q_triples(M, G, GTriples),
  partition(q_is_ground_triple, GTriples, GGround, GNonground),
  q_triples(M, H, HTriples),
  ord_subtract(HTriples, GGround, HInstance),
  rdf_graph_instance(HInstance, GNonground, [], Map).

rdf_graph_instance([], [], Map, Map) :- !.
rdf_graph_instance0(L1, [rdf(S2,P,O2)|T2], Map1, Map) :-
  % Find an instance, i.e., Specific of Generic.
  maplist(bnode_to_var, [S2,O2], [S3,O3]),
  select(rdf(S3,P,O3), L1, NewL1),
  rdf_term_instance(S3, S2, Map1, Map2),
  rdf_term_instance(O3, O2, Map2, Map3),
  rdf_graph_instance0(NewL1, T2, Map3, Map).

bnode_to_var(BNode, _) :-
  rdf_is_bnode(BNode), !.
bnode_to_var(Term, Term).



%! rdf_graph_merge(+MergingGraphs:list(atom), +MergedGraph:rdf_graph) is det.
% The operation fo **merging** takes the union after forcing any shared
%  blank nodes, which occur in more than one graph, to be distinct in each
%  graph.
% The resulting graph is called the **merge**.

rdf_graph_merge(FromGs, ToG) :-
  % Collect the shared blank nodes.
  aggregate_all(
    set(FromG1-SharedBNode),
    (
      member(FromG1, FromG2, FromGs),
      % Use the natural order of atomic names.
      % The idea is that we only replace shared blank nodes in
      % the latter graph, but not in the former.
      FromG1 @< FromG2,
      \+ (
        member(FromG3, FromGs),
        FromG1 @< FromG3,
        FromG3 @< FromG2
      ),
      rdf_bnode(FromG1, SharedBNode),
      rdf_bnode(FromG2, SharedBNode)
    ),
    SharedBNodes
  ),

  % From the shared blank nodes that will be replaced per graph
  % to the actual blank node mapping.
  findall(
    bnode_map(FromG, SharedBNode-NewBNode),
    (
      member(FromG-SharedBNode, SharedBNodes),
      qb_bnode(NewBNode)
    ),
    Map
  ),

  % Effectuate the mapping while performing the merge.
  forall(
    (
      member(FromG, FromGs),
      rdf_retractall(S1, P, O1, FromG)
    ),
    (
      (   memberchk(bnode_map(FromG,S1-S2), Map)
      ->  true
      ;   S2 = S1
      ),
      (   memberchk(bnode_map(FromG,O1-O2), Map)
      ->  true
      ;   O2 = O1
      ),
      qb(S2, P, O2, ToG)
    )
  ).



%! rdf_graph_same_size(+Graph1:atom, +Graph2:atom) is semidet.
% Succeds if the RDF graphs denoted by the given names have the same
%  number of triples.

rdf_graph_same_size(G, H) :-
  rdf_graph_get_property(G, triples(Count)),
  rdf_graph_get_property(H, triples(Count)).



%! rdf_is_ground_graph(+Graph:rdf_graph) is semidet.
% Succeeds if the given RDF graph is ground, i.e., contains no blank node.
%! rdf_is_ground_graph(-Graph:rdf_graph) is nondet.
% Enumerates RDF graphs that are ground.
%
% A **ground** RDF graph is one that contains no blank nodes.
%
% @compat RDF 1.1 Semantics

rdf_is_ground_graph(G) :-
  rdf_graph(G),
  \+ rdf_bnode(G, _).



%! rdf_proper_graph_instance(
%!   +ProperInstance:atom,
%!   +Graph:rdf_graph,
%!   -Map:list(pair(bnode,rdf_term))
%! ) is semidet.
% A **proper instance** of a graph is an instance in which a blank node
%  has been replaced by a name, or two blank nodes in the graph have
%  been mapped into the same node in the instance.
%
% @compat RDF 1.1 Semantics

rdf_proper_graph_instance(G, H, Map) :-
  rdf_graph_instance(G, H, Map),
  rdf_proper_graph_instance_map(Map).

% A blank node is mapped onto an RDF name.
rdf_proper_graph_instance_map(Map) :-
  ord_memberchk(_-Name, Map),
  rdf_is_name(Name), !.
% Two different blank nodes are mapped onto the same blank node.
rdf_proper_graph_instance_map(Map) :-
  member(BNode1-BNode3, Map),
  member(BNode2-BNode3, Map),
  BNode1 \== BNode2, !.



%! rdf_proper_subgraph(+ProperSubgraph:atom, +Graph) is semidet.
% A **proper subgraph** is a proper subset of the triples in the graph.
%
% @compat RDF 1.1 Semantics

rdf_proper_subgraph(G, H) :-
  rdf_subgraph(G, H),
  \+ rdf_graph_same_size(G, H).



%! rdf_subgraph(+Subgraph:atom, +Graph:rdf_graph) is semidet.
% A **subgraph** of an RDF graph is a subset of the triples in the graph.
%
% @compat RDF 1.1 Semantics

rdf_subgraph(G, H) :-
  \+ ((
    rdf(S, P, O, G),
    \+ rdf(S, P, O, H)
  )).



/*
% UNDER DEVELOPMENT

%! rdf_graph_sat(+I, +G:atom) is semidet.
% Interpretation I **(simply) satisfies** RDF graph E when I(E)=true.
%
% RDF graph E is (simply) satisfiable when
%  a simple interpretation I exists which satisfies it.
% Otherwise, RDF graph E is (simply) unsatisfiable.
%
% @compat RDF 1.1 Semantics


%! rdf_graph_entails(+G:atom, +E:atom) is semidet.
% RDF Graph G simply **entails** RDF graph E when every interpretation which
%  *satisfies* G also satisfies E.
%
% @compat RDF 1.1 Semantics



%! rdf_graph_equiv(+Graph1:atom, +Graph2:atom) is semidet.
% RDF graphs are **logically equivalent** iff they *entail* each other.
%
% @compat RDF 1.1 Semantics

rdf_graph_equiv(G1, G2) :-
  rdf_graph_entails(G1, G2),
  rdf_graph_entails(G2, G1).
*/
