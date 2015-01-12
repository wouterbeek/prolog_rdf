:- module(
  rdf_bnode_write,
  [
    rdf_bnode_prefix/1, % -BNodePrefix:atom
    rdf_bnode_prefix/4, % +Scheme:atom
                        % +Authority:atom
                        % +Hash:atom
                        % -BNodePrefix:atom
    rdf_bnode_write/1, % +BNode
    rdf_bnode_write/2, % +BNodePrefix:atom
                       % +BNode
    reset_bnode_admin/0
  ]
).

/** <module> RDF Blank Node

Additional Blank Node support.

@author Wouter Beek
@version 2014/06, 2014/09
*/

:- use_module(library(semweb/turtle)). % Private predicates.
:- use_module(library(uri)).

:- thread_local(bnode_counter/1).
:- thread_local(bnode_map/2).





%! rdf_bnode_map(+BNodePrefix:atom, +BNode:atom, -MappedBNode:atom) is det.

rdf_bnode_map(BNodePrefix, BNode, MappedBNode):-
  % Retrieve (existing) or create (new) a numeric blank node identifier.
  (   bnode_map(BNode, Id)
  ->  true
  ;   increment_bnode_counter(Id),
      assert(bnode_map(BNode, Id))
  ),
  
  % @tbd Vistuoso does not accept a digit as the first character in
  %      a blank node label (going against the Turtle 1.1 specification).
  atomic_list_concat([BNodePrefix,x,Id], MappedBNode).


%! rdf_bnode_prefix(-BNodePrefix:atom) is semidet.

rdf_bnode_prefix('_:').


%! rdf_bnode_prefix(
%!   +Scheme:atom,
%!   +Authority:atom,
%!   +Hash:atom,
%!   -BNodePrefix:atom
%! ) is det.

rdf_bnode_prefix(Scheme, Authority, Hash1, BNodePrefix):-
  atomic_concat(Hash1, '#', Hash2),
  atomic_list_concat(['','.well-known',genid,Hash2], '/', Path),
  uri_components(BNodePrefix, uri_components(Scheme,Authority,Path,_,_)).


%! rdf_bnode_write(+BNode:atom) is det.
% Wrapper around rdf_bnode_write/2 using the default blank node prefix.

rdf_bnode_write(BNode):-
  rdf_bnode_prefix(BNodePrefix),
  rdf_bnode_write(BNodePrefix, BNode).


%! rdf_bnode_write(+BNodePrefix:atom, +BNode:atom) is det.

rdf_bnode_write(BNodePrefix, BNode):-
  rdf_bnode_map(BNodePrefix, BNode, MappedBNode),
  
  % If the blank node is replaced by a well-known IRI,
  % then we use the predicate term writer.
  (   rdf_bnode_prefix(BNodePrefix)
  ->  write(MappedBNode)
  ;   turtle:turtle_write_uri(current_output, MappedBNode)
  ).



% Blank node administration.

increment_bnode_counter(Id2):-
  retract(bnode_counter(Id1)),
  Id2 is Id1 + 1,
  assert(bnode_counter(Id2)).


reset_bnode_admin:-
  reset_bnode_counter,
  reset_bnode_map.

reset_bnode_counter:-
  retractall(bnode_counter(_)),
  assert(bnode_counter(0)).

reset_bnode_map:-
  retractall(bnode_map(_,_)).

