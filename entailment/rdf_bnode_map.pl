:- module(
  rdf_bnode_map,
  [
    add_bnode_map/3, % +Graph:atom
                     % +BlankNode:bnode
                     % +Term:or([bnode,iri,literal])
    bnode_to_term/3, % +Graph:atom
                     % +BlankNode:bnode
                     % -Term:or([bnode,iri,literal])
    clear_bnode_map/1, % +Graph:atom
    term_to_bnode/3 % +Graph:atom
                    % +Term:or([bnode,iri,literal])
                    % -BlankNode:bnode
  ]
).

/** <module> RDF blank node maps

Since literals are not allowed to occur in the subject position
of RDF triples, blank nodes need to be associated with them in order
to be able to form propositions regarding literals.

Blank nodes and IRIs should also be mapped onto a single blank node
in simple entailment, in order to ascertain that the original graph
is a proper instance of every materialized graph.

@author Wouter Beek
@version 2013/09, 2014/06-2014/07
*/

:- use_module(library(assoc)).
:- use_module(library(semweb/rdf_db)).

%! bnode_to_term_map0(+Graph:atom, -Map:assoc) is semidet.
% The mapping from blank nodes to RDF terms.

:- dynamic(bnode_to_term_map0/2).

%! term_to_bnode_map0(+Graph:atom, -Map:assoc) is semidet.
% The mapping from RDF terms to blank nodes.

:- dynamic(term_to_bnode_map0/2).



%! add_bnode_map(
%!   +Graph:atom,
%!   +BlankNode:bnode,
%!   +RdfTerm:or([bnode,iri,literal])
%! ) is det.
% Adds the mapping between a single blank node and a single RDF term.
%
% This is stored in 2 association lists, one for each direction of the mapping.

add_bnode_map(G, B, T):-
  % Add a mapping from blank node to term.
  bnode_to_term_map(G, OldB2T),
  retractall(bnode_to_term_map0(G, OldB2T)),
  put_assoc(B, OldB2T, T, NewB2T),
  assert(bnode_to_term_map0(G, NewB2T)),

  % Add a mapping from term to blank node.
  term_to_bnode_map(G, OldT2B),
  retractall(term_to_bnode_map0(G, OldT2B)),
  put_assoc(T, OldT2B, B, NewT2B),
  assert(term_to_bnode_map0(G, NewT2B)).


%! bnode_to_term(
%!   +Graph:atom,
%!   +BlankNode:bnode,
%!   -Resource:or([bnode,iri,literal])
%! ) is semidet.
% If a mapping exists, this returns the mapped term.

bnode_to_term(G, B, T):-
  bnode_to_term_map0(G, Map),
  get_assoc(B, Map, T).

%! bnode_to_term_map(+Graph:atom, -Map:assoc) is det.

% The store already exists.
bnode_to_term_map(G, Map):-
  bnode_to_term_map0(G, Map), !.
% The store is created.
bnode_to_term_map(G, Map):-
  empty_assoc(Map),
  assert(bnode_to_term_map0(G, Map)).


%! clear_bnode_map(+Graph:atom) is det.
% Removed the bnode maps for the given graph.
%! clear_bnode_map(-Graph:atom) is det.
% Removes the bnode maps for all graphs.

clear_bnode_map(G):-
  retractall(bnode_to_term_map0(G,_)),
  retractall(term_to_bnode_map0(G,_)).


%! term_to_bnode(
%!   +Graph:atom,
%!   +Term:or([bnode,iri,literal]),
%!   -BlankNode:bnode
%! ) is det.
% Either an existing mapping is returned,
% or a new mapping is created and returned.

% The store contains a blank node that stands for the given resource.
term_to_bnode(G, T, B):-
  term_to_bnode_map0(G, Map),
  get_assoc(T, Map, B), !.
% The store does not contain a blank node that stands for the given resource,
% so a new blank node is created to stand for the given resource and this
% resource-and-blank node pair is added to the R2B mapping.
term_to_bnode(G, T, B):-
  rdf_bnode(B),
  add_bnode_map(G, B, T).


%! term_to_bnode_map(+Graph:atom, -Map:assoc) is semidet.

% The store already exists.
term_to_bnode_map(G, Map):-
  term_to_bnode_map0(G, Map), !.
% The store is created.
term_to_bnode_map(G, Map):-
  empty_assoc(Map),
  assert(term_to_bnode_map0(G, Map)).

