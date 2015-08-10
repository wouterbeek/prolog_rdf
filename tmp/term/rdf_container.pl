:- module(
  rdf_container,
  [
    rdf_alt/3, % ?Alt:or([bnode,iri])
               % -Contents:list(rdf_term)
               % ?Graph:atom
    rdf_assert_collection_member/3, % +Collection:or([bnode,iri])
                                    % +Member:rdf_term
                                    % +Graph:atom
    rdf_bag/3, % ?Bag:or([bnode,iri])
               % -Contents:list(rdf_term)
               % ?Graph:atom
    rdf_collection/3, % ?Collection:or([bnode,iri])
                      % -Contents:list(rdf_term)
                      % ?Graph:atom
    rdf_container_membership_property/2, % ?Predicate:iri
                                         % ?Index:positive_integer
    rdf_seq/3 % ?Seq:or([bnode,iri])
              % -Contents:list(rdf_term)
              % ?Graph:atom
  ]
).

/** <module> RDF container

Support for RDF containers (sequence, bag, and alternatives).

@author Wouter Beek
@tbd Add predicates for building containers.
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07-2013/09, 2014/02,
         2014/11
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta(rdf_alt(r,t,?)).
:- rdf_meta(rdf_assert_collection_member(r,r,+)).
:- rdf_meta(rdf_bag(r,-,?)).
:- rdf_meta(rdf_collection(r,-,?)).
:- rdf_meta(rdf_collection0(r,-,?)).
:- rdf_meta(rdf_collection_member(r,r,?)).
:- rdf_meta(rdf_collection_nth(?,r,r,?)).
:- rdf_meta(rdf_container_membership_property(r,?)).
:- rdf_meta(rdf_seq(r,-,?)).





%! rdf_alt(
%!   ?Alt:or([bnode,iri]),
%!   -Contents:list(rdf_term),
%!   ?Graph:atom
%! ) is nondet.
% Alternative collections.
% No duplicates and unordered.

rdf_alt(Alt, Contents, G):-
  rdfs_individual_of(Alt, rdf:'Alt'),
  rdf_subject(Alt),
  rdf_term(Alt, G),
  rdf_collection_contents(Alt, Contents, G).



%! rdf_assert_collection_member(
%!   +Collection:or([bnode,iri]),
%!   +Member:rdf_term,
%!   ?Graph:atom
%! ) is det.

rdf_assert_collection_member(Collection, Member, Graph):-
  % Since it is too expensive to calculate the number of elements
  % in a collection each time a new element is added,
  % we store this number in a global variable.
  flag(Collection, MemberId, MemberId + 1),
  atomic_concat('_', MemberId, PName),
  rdf_global_id(rdf:PName, P),
  rdf_assert2(Collection, P, Member, Graph).



%! rdf_bag(
%!   +Bag:or([bnode,iri]),
%!   -Contents:list(rdf_term),
%!   ?Graph:atom
%! ) is nondet.
% Returns bags and their contents in the given graph.
% Unordered & duplicates allowed.

rdf_bag(Bag, Contents, G):-
  rdfs_individual_of(Bag, rdf:'Bag'),
  rdf_subject(Bag),
  rdf_term(Bag, G),
  rdf_collection_contents(Bag, Contents, G).



%! rdf_collection(
%!   ?Collection:or([bnode,iri]),
%!   -Contents:list(rdf_term),
%!   ?Graph:atom
%! ) is nondet.

rdf_collection(Collection, Contents, Graph):-
  rdf_alt(Collection, Contents, Graph).
rdf_collection(Collection, Contents, Graph):-
  rdf_bag(Collection, Contents, Graph).
rdf_collection(Collection, Contents, Graph):-
  rdf_seq(Collection, Contents, Graph).



%! rdf_collection_contents(
%!   ?Collection:or([bnode,iri]),
%!   -Contents:list(rdf_term),
%!   ?Graph:atom
%! ) is nondet.

rdf_collection_contents(Collection, Contents, Graph):-
  findall(
    I-Content,
    (
      rdf(Collection, ContainerMembershipProperty, Content, Graph),
      rdf_container_membership_property(ContainerMembershipProperty, I)
    ),
    Pairs1
  ),
  % The order in which container members are read is not necessarily
  %  the indicated order (based on indices).
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, Contents).



%! rdf_collection_member(
%!   ?Member:rdf_term,
%!   ?Collection:or([bnode,iri]),
%!   ?Graph:atom
%! ) is nondet.

rdf_collection_member(Member, Collection, Graph):-
  rdf_collection_nth(_, Collection, Member, Graph).



%! rdf_collection_nth(
%!   ?Index:positive_integer,
%!   ?Collection:or([bnode,iri]),
%!   ?Member:rdf_term,
%!   ?Graph:atom
%! ) is nondet.

rdf_collection_nth(I, Collection, Member, Graph):-
  nonvar(I), !,
  rdf_container_membership_property(P, I),
  rdf(Collection, P, Member, Graph).
rdf_collection_nth(I, Collection, Member, Graph):-
  rdf(Collection, P, Member, Graph),
  rdf_container_membership_property(P, I).



%! rdf_container_membership_property(
%!    +Predicate:iri,
%!   -Index:positive_integer
%! ) is det.
%! rdf_container_membership_property(
%!   -Predicate:iri,
%!   +Index:positive_integer
%! ) is det.
%! rdf_container_membership_property(
%!   -Predicate:iri,
%!   -Index:positive_integer
%! ) is nondet.
% Succeeds if Predicate is a container membership property with Index.

% (+,?)
rdf_container_membership_property(P, Index):-
  nonvar(P), !,
  rdf_global_id(rdf:LocalName, P),
  atom_concat('_', Atom, LocalName),
  atom_number(Atom, Index),
  nonneg(Index).
% (?,+)
rdf_container_membership_property(P, Index):-
  positive_integer(Index), !,
  atom_number(Atom, Index),
  atom_concat('_', Atom, LocalName),
  rdf_global_id(rdf:LocalName, P).
% (?,?)
rdf_container_membership_property(P, Index):-
  between(1, inf, Index),
  rdf_container_membership_property(P, Index).



%! rdf_seq(
%!   ?Sequence:or([bnode,iri]),
%!   -Contents:list(rdf_term),
%!   ?Graph:atom
%! ) is nondet.
% Ordered.

rdf_seq(Seq, Contents, G):-
  rdfs_individual_of(Seq, rdf:'Seq'),
  rdf_subject(Seq),
  rdf_term(Seq, G),
  rdf_collection_contents(Seq, Contents, G).

