:- module(
  rdf_bnode_map,
  [
    add_bnode_map/3, % +Graph:atom
                     % +BlankNode:bnode
                     % +Resource:or([bnode,iri,literal])
    b2r/3, % +Graph:atom
           % +BlankNode:bnode
           % -Resource:or([bnode,iri,literal])
    r2b/3, % +Graph:atom
           % +Resource:or([bnode,iri,literal])
           % -BlankNode:bnode
    remove_bnode_map/1 % +Graph:atom
  ]
).

/** <module> RDF bnode mapping

Since literals are not allowed to occur in the subject position
of RDF triples, blank nodes need to be associated with them in order
to be able to form propositions regarding literals.

Blank nodes and IRIs should also be mapped onto a single blank node
in simple entailment, in order to ascertain that original graph
is a proper instance of the materialized graph.

@author Wouter Beek
@version 2013/09
*/

:- use_module(library(assoc)).
:- use_module(library(semweb/rdf_db)).

% The mapping between blank nodes and literals, which is used
% to assert predications of literals, even though literals cannot
% be subject terms according to RDF 1.0 syntax.
:- dynamic(b2r/2).
:- dynamic(r2b/2).



%! add_bnode_map(
%!   +Graph:atom,
%!   +BlankNode:bnode,
%!   +Resource:or([bnode,iri,literal])
%! ) is det.
% Adds the mapping between a single blank node and a single resource
% to the association lists that are used for storage.

add_bnode_map(G, B, R):-
  % Add a mapping from the blank node to the resource.
  b2r_store(G, OldB2R),
  retractall(b2r(G, OldB2R)),
  put_assoc(B, OldB2R, R, NewB2R),
  assert(b2r(G, NewB2R)),

  % Add a mapping from the resource to the blank node.
  r2b_store(G, OldR2B),
  retractall(r2b(G, OldR2B)),
  put_assoc(R, OldR2B, B, NewR2B),
  assert(r2b(G, NewR2B)).

%! b2r(
%!   +Graph:atom,
%!   +BlankNode:bnode,
%!   -Resource:or([bnode,iri,literal])
%! ) is semidet.

b2r(G, B, R):-
  b2r_store(G, A),
  get_assoc(B, A, R).

%! b2r_init(+Graph:atom) is det.
% Initialization of the B2R store for the given RDF graph.
% This predicate ensures that the store exists for the given graph.

% The store already exists.
b2r_store(G, A):-
  b2r(G, A), !.
% The store is created.
b2r_store(G, A):-
  retractall(b2r/2),
  empty_assoc(A),
  assert(b2r(G, A)).

%! r2b(
%!   +Graph:atom,
%!   +Resource:or([bnode,iri,literal]),
%!   -BlankNode:bnode
%! ) is det.

% The store contains a blank node that stands for the given resource.
r2b(G, R, B):-
  r2b_store(G, A),
  get_assoc(R, A, B), !.
% The store does not contain a blank node that stands for the given resource,
% so a new blank node is created to stand for the given resource and this
% resource-and-blank node pair is added to the R2B mapping.
r2b(G, R, B):-
  rdf_bnode(B),
  add_bnode_map(G, B, R).

%! r2b_init(+Graph:atom) is det.
% Initialization of the R2B store for the given RDF graph.
% This predicate ensures that the store exists for the given graph.

% The store already exists.
r2b_store(G, A):-
  r2b(G, A), !.
% The store is created.
r2b_store(G, A):-
  retractall(r2b/2),
  empty_assoc(A),
  assert(r2b(G, A)).

%! remove_bnode_map(+Graph:atom) is det.

remove_bnode_map(G):-
  retractall(b2r(G,_)),
  retractall(r2b(G,_)).

