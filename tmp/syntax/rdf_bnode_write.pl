:- module(
  rdf_bnode_write,
  [
    rdf_bnode_prefix/4, % +Scheme:atom
                        % +Authority:atom
                        % +Hash:atom
                        % -BNodePrefix:atom
    rdf_bnode_prefix_write/1, % +BNode
    rdf_bnode_prefix_write/2 % +BNodePrefix:atom
                             % +BNode
  ]
).

/** <module> RDF Blank Node Prefixes

Support for writing blank node using set prefixes.

@author Wouter Beek
@version 2014/06, 2014/09, 2015/01-2015/02
*/

:- use_module(library(semweb/turtle)). % Private predicates.
:- use_module(library(uri)).



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


%! rdf_bnode_prefix_write(+BNode) is det.
% Wrapper around rdf_bnode_prefix_write/2 using the default blank node prefix.

rdf_bnode_prefix_write(BNode):-
  rdf_bnode_prefix(BNodePrefix),
  rdf_bnode_prefix_write(BNodePrefix, BNode).


%! rdf_bnode_prefix_write(+BNodePrefix:atom, +BNode) is det.

rdf_bnode_prefix_write(BNodePrefix, BNode):-
  rdf_bnode_prefix_map(BNodePrefix, BNode, MappedBNode),
  
  % If the blank node is replaced by a well-known IRI,
  % then we use the predicate term writer.
  (   rdf_bnode_prefix(BNodePrefix)
  ->  write(MappedBNode)
  ;   turtle:turtle_write_uri(current_output, MappedBNode)
  ).
