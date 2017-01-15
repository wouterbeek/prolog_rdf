:- module(
  rdf_instance,
  [
    rdf_term_instance/3, % +Specific:compound
                         % +Generic:compound
                         % -Map:list(pair(bnode,rdf_term))
    rdf_term_instance/4, % +Specific:compound
                         % +Generic:compound
                         % +Map:list(pair(bnode,rdf_term))
                         % -NewMap:list(pair(bnode,rdf_term))
    rdf_term_variant/2, % +Term1:rdf_term
                        % +Term1:rdf_term
    rdf_triple_instance/3, % +Specific:compound
                           % +Generic:compound
                           % -NewMap:list(pair(bnode,rdf_term))
    rdf_triple_instance/4, % +Specific:compound
                           % +Generic:compound
                           % +Map:list(pair(bnode,rdf_term))
                           % -NewMap:list(pair(bnode,rdf_term))
    rdf_triple_variant/2 % +Triple1:compound
                         % +Triple2:compound
  ]
).

/** <module> RDF: Term and triple instances

Support for blank node mappings in RDF.

@author Wouter Beek
@version 2014/11
*/

:- use_module(library(ordsets)).

:- rdf_meta(rdf_term_variant(o,o)).
:- rdf_meta(rdf_term_instance(o,o,-)).
:- rdf_meta(rdf_term_instance(o,o,+,-)).
:- rdf_meta(rdf_triple_instance(t,t,-)).
:- rdf_meta(rdf_triple_instance(t,t,+,-)).
:- rdf_meta(rdf_triple_variant(t,t)).





% rdf_term_instance(
%!   +Specific:compound,
%!   +Generic:compound,
%!   -NewMap:list(pair(bnode,rdf_term))
%! ) is det.
%! rdf_term_instance(
%!   +Specific:compound,
%!   +Generic:compound,
%!   +Map:list(pair(bnode,rdf_term)),
%!   -NewMap:list(pair(bnode,rdf_term))
%! ) is det.

rdf_term_instance(Specific, Generic, NewMap) :-
  rdf_term_instance(Specific, Generic, [], NewMap).

% Generic and Specific are the same and are possibly a blank node.
rdf_term_instance(Name, Name, Map, Map) :- !.
% Generic is a known blank node, use the existing mapping.
rdf_term_instance(Term, BNode, Map, Map) :-
  ord_memberchk(BNode-Term, Map), !.
% Generic is a new blank node, add it to the mapping.
rdf_term_instance(Term, BNode, Map1, Map2) :-
  rdf_is_bnode(BNode),
  ord_add_element(Map1, BNode-Term, Map2).



%! rdf_term_variant(+Term1:rdf_term, +Term1:rdf_term) is semidet.
% Succeeds if the given RDF terms are variants.
%
% Similar to Prolog term variants, i.e. =@=/2.

rdf_term_variant(BNode1, BNode2) :-
  rdf_is_bnode(BNode1),
  rdf_is_bnode(BNode2), !.
rdf_term_variant(Term, Term).



%! rdf_triple_instance(
%!   +Specific:compound,
%!   +Generic:compound,
%!   -NewMap:list(pair(bnode,rdf_term))
%! ) is det.
%! rdf_triple_instance(
%!   +Specific:compound,
%!   +Generic:compound,
%!   +Map:list(pair(bnode,rdf_term)),
%!   -NewMap:list(pair(bnode,rdf_term))
%! ) is det.
% Returns a blank node mapping that maps blank nodes from Generic onto
%  terms in Specific so that Generic == Specific.

rdf_triple_instance(Specific, Generic, NewMap) :-
  rdf_triple_instance(Specific, Generic, [], NewMap).

rdf_triple_instance(rdf(S2,P,O2), rdf(S1,P,O1), Map1, Map3) :-
  % Example 1: `_:y -> ex:a`.
  % Example 2: `_:x -> ex:a`.
  rdf_term_instance(S2, S1, Map1, Map2),
  % Example 1: `_:x -> _:x`.
  % Example 2: `_:x -> ex:a`.
  rdf_term_instance(O2, O1, Map2, Map3).



%! rdf_triple_variant(+Triple1:compound, +Triple1:compound) is semidet.
% Succeeds if the given RDF triples are variants.
%
% Similar to Prolog term variants, i.e. =@=/2.

rdf_triple_variant(rdf(S1,P,O1), rdf(S2,P,O2)) :-
  rdf_term_variant(S1, S2),
  rdf_term_variant(O1, O2).
