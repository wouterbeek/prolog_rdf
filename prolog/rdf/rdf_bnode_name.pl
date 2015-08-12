:- module(
  rdf_bnode_name,
  [
    rdf_bnode_name/2, % +BlankNode:bnode
                      % -Name:atom
    reset_bnode_names/0
  ]
).

/** <module> RDF blank node name

Consistent naming of blank nodes.

@author Wouter Beek
@version 2015/08
*/

%! bnode_name_counter(+Id:nonneg) is semidet.
%! bnode_name_counter(-Id:nonneg) is det.

:- thread_local(bnode_name_counter/1).

%! bnode_name_map(+BlankNode:bnode, +Id:nonneg) is semidet.
%! bnode_name_map(+BlankNode:bnode, -Id:nonneg) is semidet.
%! bnode_name_map(-BlankNode:bnode, +Id:nonneg) is semidet.
%! bnode_name_map(-BlankNode:bnode, -Id:nonneg) is nondet.

:- thread_local(bnode_name_map/2).





%! rdf_bnode_name(+BlankNode:bnode, -Name:atom) is det.
% Name is a blank node name for BlankNode that is compliant with
% the Turtle grammar.
%
% It is assured that within one session the same blank node
% will receive the same blank node name.
%
% The prefix `_:` is used.

rdf_bnode_name(BNode, Name):-
  rdf_bnode_name0('_:', BNode, Name).

%! rdf_bnode_name0(+Prefix:atom, +BlankNode:bnode, -Name:atom) is det.

rdf_bnode_name0(Prefix, BNode, Name):-
  % Retrieve (existing) or create (new) a numeric blank node identifier.
  (   bnode_name_map(BNode, Id)
  ->  true
  ;   increment_bnode_name_counter(Id),
      assert(bnode_name_map(BNode, Id))
  ),
  atomic_concat(Prefix, Id, Name).

increment_bnode_name_counter(Id):-
  retract(bnode_name_counter(Id0)), !,
  Id is Id0 + 1,
  assert(bnode_name_counter(Id)).
increment_bnode_name_counter(Id):-
	reset_bnode_name_counter,
  increment_bnode_name_counter(Id).

reset_bnode_names:-
  reset_bnode_name_counter,
  reset_bnode_name_map.

reset_bnode_name_counter:-
  retractall(bnode_name_counter(_)),
  assert(bnode_name_counter(1)).

reset_bnode_name_map:-
  retractall(bnode_name_map(_,_)).
