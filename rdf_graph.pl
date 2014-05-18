:- module(
  rdf_graph,
  [
    rdf_graph/2, % +ComplexGraph:compound
                 % -SimpleGraph:atom
    rdf_graph_copy/2, % +From:atom
                      % +To:atom
    rdf_graph_equivalence/2, % +Graph1:atom
                             % +Graph2:atom
    rdf_graph_instance/3, % +Graph1:atom
                          % +Graph2:atom
                          % -BNodeMap:list(pair(bnode,or([iri,literal])))
    rdf_graph_proper_instance/3, % +Graph1:atom
                                 % +Graph2:atom
                                 % -BNodeMap:list(pair(bnode,or([iri,literal])))
    rdf_graph_merge/2, % +MergingGraphs:list(atom)
                       % +MergedGraph:atom
    rdf_graph_to_triples/2, % +Graph:atom
                            % -Triples:list(compound)
    rdf_graphs_to_graph/2, % +FromGraphs:list(atom)
                           % +ToGraph:atom
    rdf_ground/1, % +Graph:atom
    rdf_ground/1, % +Triple:compound
    rdf_is_graph/2, % +Graph:atom
                    % -NormalizedGraph:atom
    rdf_same_graph/2, % +Graph1:atom
                      % +Graph2:atom
    rdf_schema/4, % +Graph:atom
                  % -RdfsClasses:ordset(iri)
                  % -RdfProperties:ordset(iri)
                  % -Triples:ordset(compound)
    rdf_subgraph/2, % +Graph1:atom
                    % +Graph2:atom
    rdf_triple/4 % ?Subject:or([bnode,iri])
                 % ?Predicate:iri
                 % ?Object:or([bnode,literal,iri])
                 % ?Triple:triple
  ]
).

/** <module> RDF graph

Predicates that apply to entire RDF graphs.

@author Wouter Beek
@see Graph theory support for RDF is found in module rdf_graph_theory.pl.
@see For conversions from/to serialization formats, see module rdf_serial.pl.
@tbd How to do backward chaining in query/[3,4]?
@version 2012/01-2013/05, 2013/07-2013/08, 2013/11, 2014/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(list_ext)).
:- use_module(rdf(rdf_graph_name)).
:- use_module(rdf_term(rdf_term)).

:- rdf_meta(rdf_triple(r,r,r,?)).



%! rdf_bnode_replace(
%!   +Triple1:compound,
%!   +Map:list(pair(bnode,or([literal,iri,var]))),
%!   -Triple2:compound
%! ) is det.
% Replaces bnodes in triples with variables.

rdf_bnode_replace(rdf(S1,P,O1), Map, rdf(S2,P,O2)):- !,
  rdf_bnode_replace(S1, Map, S2),
  rdf_bnode_replace(O1, Map, O2).
% Not a blank node, do not replace.
rdf_bnode_replace(X, _Map, X):-
  \+ rdf_is_bnode(X), !.
% A blank node that is in the mapping.
rdf_bnode_replace(X, Map, Y):-
  memberchk(X-Y, Map), !.
% A blank node that is not in the mapping, replace with a Prolog variable.
rdf_bnode_replace(_X, _Map, _Var).

%! rdf_graph(+ComplexGraph:or([atom,compound]), -SimpleGraph:atom) is det.

rdf_graph(G2:_RowNumber, G2):- !.
rdf_graph(G, G).

%! rdf_graph_copy(+From:atom, +To:atom) is det.
% Copying a graph is the same as merging a single graph
% and storing the result under a new name.

rdf_graph_copy(From, To):-
  From \== To,
  rdf_graph_merge([From], To).

%! rdf_graph_equivalence(+Graph1:atom, +Graph2:atom) is semidet.

rdf_graph_equivalence(Graph1, Graph2):-
  rdf_graph_equivalence0(Graph1, Graph2),
  rdf_graph_equivalence0(Graph2, Graph1).
rdf_graph_equivalence0(Graph1, Graph2):-
  forall(
    rdf(Subject1, Predicate, Object1, Graph1),
    (
      rdf(Subject2, Predicate, Object2, Graph2),
      rdf_graph_equivalence_subject0(Graph1, Subject1, Graph2, Subject2),
      rdf_graph_equivalence_object0(Graph1, Object1, Graph2, Object2)
    )
  ).
rdf_graph_equivalence_subject0(_Graph1, Subject, _Graph2, Subject):-
  rdf_is_resource(Subject), !.
rdf_graph_equivalence_subject0(Graph1, Subject1, Graph2, Subject2):-
  bnode_translation0(Graph1, Subject1, Graph2, Subject2).
rdf_graph_equivalence_object0(_Graph1, Object, _Graph2, Object):-
  rdf_is_resource(Object), !.
rdf_graph_equivalence_object0(_Graph1, Object, _Graph2, Object):-
  rdf_is_literal(Object), !.
rdf_graph_equivalence_object0(Graph1, Object1, Graph2, Object2):-
  bnode_translation0(Graph1, Object1, Graph2, Object2).
bnode_translation0(G1, Resource1, G2, Resource2):-
  maplist(rdf_bnode, [Resource1,Resource2], [G1,G2]), !.

rdf_graph_instance(G, H, Map):-
  rdf_graph(G), rdf_graph(H),
  rdf_triples(G, L1), rdf_triples(H, L2),
  rdf_graph_instance(L2, L1, [], Map).

rdf_graph_instance([], _L2, SolMap, SolMap):- !.
rdf_graph_instance([S1-P-O1|T1], L2, Map, SolMap):-
  rdf_bnode_replace(S1, Map, S2),
  (var(S2) -> MapExtension1 = [S1-S2] ; MapExtension1 = []),
  rdf_bnode_replace(O1, Map, O2),
  (var(O2) -> MapExtension2 = [O1-O2] ; MapExtension2 = []),
  member(S2-P-O2, L2),
  append([Map,MapExtension1,MapExtension2], NewMap),
  rdf_graph_instance(T1, L2, NewMap, SolMap).

%! rdf_graph_merge(+MergingGraphs:list(atom), +MergedGraph:atom) is det.
%! rdf_graph_merge(+MergingGraphs:list(atom), -MergedGraph:atom) is det.
% Merges RDF graphs.
% When merging RDF graphs we have to make sure that their blank nodes are
% standardized apart.

rdf_graph_merge(Gs, MergedG):-
  % Type checking.
  is_list(Gs),
  maplist(rdf_graph, Gs),

  % Generate a name for the merged graph, if needed.
  rdf_new_graph(MergedG),

  % Collect the shared blank nodes.
  findall(
    G1/G2/SharedBNode,
    (
      member(G1, G2, Gs),
      % Use the natural order of atomic names.
      % The idea is that we only replace shared blank nodes in
      % the latter graph.
      G1 @< G2,
      rdf_bnode(SharedBNode, G1),
      rdf_bnode(SharedBNode, G2)
    ),
    SharedBNodes
  ),

  % Replace the blank nodes.
  (
    SharedBNodes == []
  ->
    rdf_graphs_to_graph(Gs, MergedG)
  ;
    forall(
      (
        member(G, Gs),
        rdf(S, P, O, G)
      ),
      (
        rdf_bnode_replace(
          SharedBNodes,
          rdf(S, P, O, G),
          rdf(NewS, P, NewO)
        ),
        rdf_assert(NewS, P, NewO, MergedG)
      )
    )
  ).

%! rdf_graph_proper_instance(
%!   +Graph1:atom,
%!   +Graph2:atom,
%!   -BNodeMap:list(pair(bnode,or([literal,iri])))
%! ) is semidet.
% A proper instance of a graph is an instance in which a blank node
% has been replaced by a name, or two blank nodes in the graph have
% been mapped into the same node in the instance.

rdf_graph_proper_instance(G, H, Map):-
  rdf_graph_instance(G, H, Map),
  (
    % A node is mapped onto an RDF name.
    member(_-X, Map),
    rdf_name(X, G)
  ;
    % Two different blank nodes are mapped onto the same blank node.
    member(X1-Y, Map),
    member(X2-Y, Map),
    X1 \== X2
  ), !.

%! rdf_graph_to_triples(+Graph:atom, -Triples:list(rdf_triple)) is det.
% Returns an unsorted list containing all the triples in a graph.
%
% @arg In The atomic name of a loaded RDF graph, or a URI.
% @arg Triples A list of triple compound term.

rdf_graph_to_triples(G, Ts):-
  rdf_graph(G), !,
  findall(rdf(S,P,O), rdf(S, P, O, G), Ts).

rdf_graphs_to_graph(G1s, G2):-
  forall(
    (
      member(G1, G1s),
      rdf(S, P, O, G1)
    ),
    rdf_assert(S, P, O, G2)
  ).

%! rdf_ground(+Graph:graph) is semidet.
% Succeeds if the given graph is ground, i.e., contains no blank node.
%! rdf_ground(+Triple) is semidet.
% Succeeds if the given triple is ground, i.e., contains no blank node.
% The predicate cannot be a blank node by definition.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_ground(rdf(S,_,O)):- !,
  \+ rdf_is_bnode(S),
  \+ rdf_is_bnode(O).
rdf_ground(G):-
  forall(
    rdf(S, P, O, G),
    rdf_ground(rdf(S,P,O))
  ).

rdf_is_graph(Graph:_, Graph):-
  atom(Graph),
  rdf_graph(Graph), !.
rdf_is_graph(Graph, Graph):-
  atom(Graph),
  rdf_graph(Graph).

rdf_same_graph(Graph1, Graph2):-
  rdf_is_graph(Graph1, Graph),
  rdf_is_graph(Graph2, Graph).

rdf_schema(G, RdfsClasses, RdfProperties, Triples):-
  aggregate_all(
    set(C),
    rdfs_individual_of(C, rdfs:'Class'),
    RdfsClasses
  ),
  aggregate_all(
    set(P),
    rdfs_individual_of(P, rdf:'Property'),
    RdfProperties
  ),
  ord_union(RdfsClasses, RdfProperties, Vocabulary),
  aggregate_all(
    set(rdf(S,P,O)),
    (
      member(S, O, Vocabulary),
      rdf(S, P, O, G)
    ),
    Triples
  ).

%! rdf_subgraph(+Graph1:atom, +Graph2:atom) is semidet.
% Succeeds if the former graph is a subgraph of the latter.
%
% @see RDF Semantics http://www.w3.org/TR/2004/REC-rdf-mt-20040210/

rdf_subgraph(G, H):-
  rdf_graph(G),
  rdf_graph(H), !,
  \+ (rdf(S, P, O, G),
  \+ rdf(S, P, O, H)).

rdf_triple(S1, P1, O1, Triple):-
  var(Triple), !,
  maplist(rdf_global_id, [S1,P1,O1], [S2,P2,O2]),
  Triple = rdf(S2,P2,O2).
rdf_triple(S, P, O, rdf(S,P,O)).

