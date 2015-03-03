:- module(
  rdf_graph,
  [
    rdf_fresh_graph/2, % ?Graph:atom
                       % +FreshnessLifetime:between(0.0,inf)
    rdf_graph_age/2, % ?Graph:atom
                     % -Age:between(0.0,inf)
    rdf_graph_instance/3, % +Instance:atom
                          % +Graph:atom
                          % -Map:list(pair(bnode,rdf_term))
    rdf_graph_merge/2, % +MergingGraphs:list(atom)
                       % +MergedGraph:atom
    rdf_graph_same_size/2, % +Graph1:atom
                           % +Graph2:atom
    rdf_is_graph/1, % @Term
    rdf_is_ground_graph/1, % +Graph:atom
    rdf_proper_graph_instance/3, % +Graph1:atom
                                 % +Graph2:atom
                                 % -BNodeMap:list(pair(bnode,or([iri,literal])))
    rdf_proper_subgraph/2, % +ProperSubgraph:atom
                           % +Graph:atom
    rdf_stale_graph/2, % ?Graph:atom
                       % +FreshnessLifetime:between(0.0,inf)
    rdf_subgraph/2, % +ProperSubgraph:atom
                    % +Graph:atom
    rdf_unload_graphs/0
  ]
).

/** <module> RDF graph

@author Wouter Beek
@compat [RDF 1.1 Semantics](http://www.w3.org/TR/2014/REC-rdf11-mt-20140225/)
@version 2012/01-2013/05, 2013/07-2013/08, 2013/11, 2014/04-2014/05, 2014/11,
         2015/03
*/

:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(generics/list_ext)).
:- use_module(plc(math/math_ext)).

:- use_module(plRdf(rdf_triples)).
:- use_module(plRdf(term/rdf_instance)).
:- use_module(plRdf(term/rdf_term)).





%! rdf_fresh_graph(
%!   +Graph:atom,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.
% Succeeds if the age of the given RDF graph is under the given freshness
%  lifetime.
%! rdf_fresh_graph(
%!   -Graph:atom,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is nondet.
% Enumerates the RDF graphs that are fresh w.r.t. the given freshness
%  lifetime.

rdf_fresh_graph(Graph, FreshnessLifetime):-
  rdf_graph_age(Graph, Age),
  is_fresh_age(Age, FreshnessLifetime).



%! rdf_graph_age(+Graph:atom, -Age:between(0.0,inf)) is det.
% Returns the age of the RDF graph with the given name in seconds.
%! rdf_graph_age(-Graph:atom, -Age:between(0.0,inf)) is nondet.
% Enumerates the currently loaded RDF graphs and their age in seconds.

rdf_graph_age(Graph, Age):-
  rdf_graph_property(Graph, source_last_modified(LastModified)),
  get_time(Now),
  Age is Now - LastModified.



%! rdf_graph_instance(
%!   +Instance:atom,
%!   +Graph:atom,
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

rdf_graph_instance(H, G, Map):-
  rdf_triples(G, GTriples),
  partition(rdf_is_ground_triple, GTriples, GGround, GNonground),
  rdf_triples(H, HTriples),
  ord_subtract(HTriples, GGround, HInstance),
  rdf_graph_instance(HInstance, GNonground, [], Map).

rdf_graph_instance([], [], Map, Map):- !.
rdf_graph_instance0(L1, [rdf(S2,P,O2)|T2], Map1, Map):-
  % Find an instance, i.e., Specific of Generic.
  maplist(bnode_to_var, [S2,O2], [S3,O3]),
  select(rdf(S3,P,O3), L1, NewL1),
  rdf_term_instance(S3, S2, Map1, Map2),
  rdf_term_instance(O3, O2, Map2, Map3),
  rdf_graph_instance0(NewL1, T2, Map3, Map).

bnode_to_var(BNode, _):-
  rdf_is_bnode(BNode), !.
bnode_to_var(Term, Term).



%! rdf_graph_merge(+MergingGraphs:list(atom), +MergedGraph:atom) is det.
% The operation fo **merging** takes the union after forcing any shared
%  blank nodes, which occur in more than one graph, to be distinct in each
%  graph.
% The resulting graph is called the **merge**.

rdf_graph_merge(FromGs, ToG):-
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
      rdf_bnode(SharedBNode, FromG1),
      rdf_bnode(SharedBNode, FromG2)
    ),
    SharedBNodes
  ),

  % From the shared blank nodes that will be replaced per graph
  %  to the actual blank node mapping.
  findall(
    bnode_map(FromG, SharedBNode-NewBNode),
    (
      member(FromG-SharedBNode, SharedBNodes),
      rdf_bnode(NewBNode)
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
      rdf_assert(S2, P, O2, ToG)
    )
  ).



%! rdf_graph_same_size(+Graph1:atom, +Graph2:atom) is semidet.
% Succeds if the RDF graphs denoted by the given names have the same
%  number of triples.

rdf_graph_same_size(G, H):-
  rdf_graph_property(G, triples(Count)),
  rdf_graph_property(H, triples(Count)).



%! rdf_is_graph(@Term) is semidet.
% rdf_graph/1 throws an exception for any non-atomic nonvar argument,
% whereas this predicate fails silently.
%
% The name of this predicate is in line with rdf_is_bnode/1, rdf_is_literal/1,
%  and rdf_is_resource/1 in [library(semweb/rdf_db)].

rdf_is_graph(Graph):-
  atom(Graph),
  rdf_graph(Graph).



%! rdf_is_ground_graph(+Graph:atom) is semidet.
% Succeeds if the given RDF graph is ground, i.e., contains no blank node.
%! rdf_is_ground_graph(-Graph:atom) is nondet.
% Enumerates RDF graphs that are ground.
%
% A **ground** RDF graph is one that contains no blank nodes.
%
% @compat RDF 1.1 Semantics

rdf_is_ground_graph(Graph):-
  rdf_graph(Graph),
  \+ rdf_bnode(_, Graph).



%! rdf_proper_graph_instance(
%!   +ProperInstance:atom,
%!   +Graph:atom,
%!   -Map:list(pair(bnode,rdf_term))
%! ) is semidet.
% A **proper instance** of a graph is an instance in which a blank node
%  has been replaced by a name, or two blank nodes in the graph have
%  been mapped into the same node in the instance.
%
% @compat RDF 1.1 Semantics

rdf_proper_graph_instance(G, H, Map):-
  rdf_graph_instance(G, H, Map),
  rdf_proper_graph_instance_map(Map).

% A blank node is mapped onto an RDF name.
rdf_proper_graph_instance_map(Map):-
  ord_memberchk(_-Name, Map),
  rdf_is_name(Name), !.
% Two different blank nodes are mapped onto the same blank node.
rdf_proper_graph_instance_map(Map):-
  member(BNode1-BNode3, Map),
  member(BNode2-BNode3, Map),
  BNode1 \== BNode2, !.



%! rdf_proper_subgraph(+ProperSubgraph:atom, +Graph) is semidet.
% A **proper subgraph** is a proper subset of the triples in the graph.
%
% @compat RDF 1.1 Semantics

rdf_proper_subgraph(G, H):-
  rdf_subgraph(G, H),
  \+ rdf_graph_same_size(G, H).



%! rdf_stale_graph(
%!   +Graph:atom,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.
% Succeeds if the age of the given RDF graph is over the given freshness
%  lifetime.
%! rdf_stale_graph(
%!   -Graph:atom,
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is nondet.
% Enumerates the RDF graphs that are stale w.r.t. the given freshness
%  lifetime.

rdf_stale_graph(Graph, FreshnessLifetime):-
  rdf_graph_age(Graph, Age),
  is_stale_age(Age, FreshnessLifetime).



%! rdf_subgraph(+Subgraph:atom, +Graph:atom) is semidet.
% A **subgraph** of an RDF graph is a subset of the triples in the graph.
%
% @compat RDF 1.1 Semantics

rdf_subgraph(G, H):-
  \+ ((
    rdf(S, P, O, G),
    \+ rdf(S, P, O, H)
  )).



rdf_unload_graphs:-
  rdf_graph(G),
  rdf_unload_graph(G),
  fail.
rdf_unload_graphs.



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

rdf_graph_equiv(G1, G2):-
  rdf_graph_entails(G1, G2),
  rdf_graph_entails(G2, G1).
*/
