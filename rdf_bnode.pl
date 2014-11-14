:- module(
  rdf_bnode,
  [
    rdf_bnode_replace/2 % +Graph:atom
                        % +OtherGraphs:list(atom)
  ]
).

/** <module> RDF: Blank node

Support for blank node terms in RDF.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(apply)).
:- use_module(library(semweb/rdf_db)).

:- thread_local(bnode_map/3).



%! rdf_bnode_replace(+Graph:atom, +OtherGraphs:list(atom)) is det.
% Replaces any blank nodes that occur in Graph and
% at least one of the OtherGraphs.

rdf_bnode_replace(G, OGs):-
  forall(
    rdf(S1, P, O1, G),
    (
      maplist(rdf_bnode_replace_term(OGs), [S1,O1], [S2,O2]),
      rdf_replace_triple(rdf(S1,P,O1,G), rdf(S2,P,O2,G))
    )
  ),
  retractall(bnode_map(G-OGs,_,_)).



% HELPERS

%! rdf_bnode_replace_term(
%!   +OtherGraphs:list(atom),
%!   +OldTerm:or([bnode,iri,literal])
%!   -NewTerm:or([bnode,iri,literal])
%! ) is det.

rdf_bnode_replace_term(OGs, S1, S2):-
  rdf_is_bnode(S1),
  member(OG, OGs),
  (   rdf(S1, _, _)
  ;   rdf(_, _, S1)
  ), !,
  (   bnode_map(G-OGs, S1, S2)
  ->  true
  ;   rdf_bode(S2),
      assert(bnode_map(G-OGs, S1, S2))
  ).
rdf_bnode_replace_term(_, S, S).



%! rdf_replace_triple(+OldTriple:compound, +NewTriple:compound) is det.

rdf_replace_triple(rdf(S,P,O,G), rdf(S,P,O,G)):- !.
rdf_replace_triple(rdf(S1,P,O1,G), rdf(S2,P,O2,G)):-
  rdf_retractall(S1, P, O1, G),
  rdf_assert(S2, P, O2, G).
