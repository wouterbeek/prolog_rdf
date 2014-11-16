:- module(
  rdf_bnode_map,
  [
    add_bnode_map/3, % ?Graph:atom
                     % +BNode:bnode
                     % +Term:rdf_term
    bnode_to_term/3, % ?Graph:atom
                     % +BNode:bnode
                     % -Term:rdf_term
    clear_bnode_map/1, % ?Graph:atom
    term_get_bnode/3, % ?Graph:atom
                      % +Term:rdf_term
                      % -BNode:bnode
    term_set_bnode/3 % ?Graph:atom
                     % +Term:rdf_term
                     % -BNode:bnode
  ]
).

/** <module> RDF term: Blank node mapping

Since literals are not allowed to occur in the subject position
 of RDF triples, blank nodes need to be associated with them in order
 to be able to form propositions that state things *about* literals.

Blank nodes and IRIs should also be mapped onto a single blank node
 in simple entailment, in order to ascertain that the original graph
 is a proper instance of every materialized graph.

The graph argument is required in blank node maps,
 since identical blank node labels that occur in different graphs may denote
 different resources.

@author Wouter Beek
@version 2013/09, 2014/06-2014/07, 2014/11
*/

:- use_module(library(assoc)).
:- use_module(library(semweb/rdf_db)).

:- use_module(generics(meta_ext)).

:- use_module(plRdf(graph/rdf_graph)).

%! bnode_to_term_map0(+Graph:atom, -Map:assoc) is semidet.
% The mapping from blank nodes to RDF terms.

:- dynamic(bnode_to_term_map0/2).

%! term_to_bnode_map0(+Graph:atom, -Map:assoc) is semidet.
% The mapping from RDF terms to blank nodes.

:- dynamic(term_to_bnode_map0/2).



%! add_bnode_map(?Graph:atom, +BNode:bnode, +Term:rdf_term) is det.
% Adds the mapping between a single blank node and a single RDF term.
%
% This is stored in 2 association lists, one for each direction of the mapping.
%
% Mappings are relative to graphs.

add_bnode_map(Graph, BNode, Term):-
  rdf_graph(Graph),
  with_mutex(rdf_bnode_map, (
    % Add a mapping from blank node to term.
    bnode_to_term_map(Graph, OldB2T),
    retractall(bnode_to_term_map0(Graph, OldB2T)),
    put_assoc(BNode, OldB2T, Term, NewB2T),
    assert(bnode_to_term_map0(Graph, NewB2T)),

    % Add a mapping from term to blank node.
    term_to_bnode_map(Graph, OldT2B),
    retractall(term_to_bnode_map0(Graph, OldT2B)),
    put_assoc(Term, OldT2B, BNode, NewT2B),
    assert(term_to_bnode_map0(Graph, NewT2B))
  )).



%! bnode_to_term(?Graph:atom, +BNode:bnode, -Term:rdf_term) is semidet.
% Returns the RDF term to which the given blank node was mapped, if any.

bnode_to_term(Graph, BNode, Term):-
  rdf_graph(Graph),
  with_mutex(rdf_bnode_map, (
    bnode_to_term_map0(Graph, Map),
    get_assoc(BNode, Map, Term)
  )).



%! clear_bnode_map(+Graph:atom) is det.
% Removes the blank node maps for the given graph.

clear_bnode_map(Graph):-
  rdf_graph(Graph),
  rdf_is_graph(Graph),
  with_mutex(rdf_bnode_map, (
    retractall(bnode_to_term_map0(Graph,_)),
    retractall(term_to_bnode_map0(Graph,_))
  )).



%! term_get_bnode(+Graph:atom, +Term:rdf_term, -BNode:bnode) is det.

term_get_bnode(Graph, Term, BNode):-
  rdf_graph(Graph),
  with_mutex(rdf_bnode_map, (
    (   term_to_bnode_map0(Graph, Map),
        get_assoc(Term, Map, BNode)
    )
  )).



%! term_set_bnode(+Graph:atom, +Term:rdf_term, -BNode:bnode) is det.
% Either an existing mapping is returned,
%  or a new mapping is created and returned.

% The store contains a blank node that stands for the given resource.
term_set_bnode(Graph, Term, BNode):-
  rdf_graph(Graph),
  with_mutex(rdf_bnode_map, (
    (   term_to_bnode_map0(Graph, Map),
        get_assoc(Term, Map, BNode)
    ->  true
    ;   % The store does not contain a blank node that stands for
        %  the given resource, so a new blank node is created to stand for
        %  the given resource and this resource-and-blank-node-pair is added
        %  to the B2R and R2B mappings.
        rdf_bnode(BNode),
        add_bnode_map(Graph, BNode, Term)
    )
  )).





% HELPERS

%! bnode_to_term_map(+Graph:atom, -Map:assoc) is det.
% Returns the blank-node-to-RDF-term-map for the given graph.
%
% This predicate ensures that the map exists.

% The store already exists.
bnode_to_term_map(Graph, Map):-
  with_mutex(rdf_bnode_map, (
    (   bnode_to_term_map0(Graph, Map)
    ->  true
    ;   % The store is created.
        empty_assoc(Map),
        assert(bnode_to_term_map0(Graph, Map))
    )
  )).



%! term_to_bnode_map(+Graph:atom, -Map:assoc) is semidet.
% Returns the term-to-blank-node-mapping for the given graph.
%
% This ensures that the mapping exists.

% The store already exists.
term_to_bnode_map(Graph, Map):-
  with_mutex(rdf_bnode_map, (
    (   term_to_bnode_map0(Graph, Map)
    ->  true
    ;   % The store is created.
        empty_assoc(Map),
        assert(term_to_bnode_map0(Graph, Map))
    )
  )).

