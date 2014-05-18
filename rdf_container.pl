:- module(
  rdf_container,
  [
    rdf_alt/3, % ?Alt:iri
               % -Contents:list(iri)
               % ?Graph:atom
    rdf_assert_collection_member/3, % +Collection:iri
                                    % +Member:iri
                                    % +Graph:atom
    rdf_bag/3, % ?Bag:iri
               % -Contents:list(iri)
               % ?Graph:atom
    rdf_collection/3, % ?Collection:iri
                      % -Contents:list(iri)
                      % ?Graph:atom
    rdf_collection_member/3, % ?Member:or([bnode,iri])
                             % ?Collection:or([bnode,iri])
                             % ?Graph
    rdf_collection_nth/4, % ?Index:positive_integer,
                          % ?Collection:iri,
                          % ?Member:iri,
                          % ?Graph:atom
    rdf_container_membership_property/2, % ?Predicate:iri
                                         % ?Index:positive_integer
    rdf_seq/3 % ?Seq:iri
              % -Contents:list(iri)
              % ?Graph:atom
  ]
).

/** <module> RDF container

Support for RDF containers (sequence, bag, and alternatives).

@author Wouter Beek
@tbd Add predicates for building containers.
@version 2011/08-2012/03, 2012/09, 2012/11-2013/03, 2013/07-2013/09, 2014/02
*/

:- use_module(generics(typecheck)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf_term(rdf_term)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').

:- rdf_meta(rdf_alt(r,-,?)).
:- rdf_meta(rdf_assert_collection_member(r,r,+)).
:- rdf_meta(rdf_bag(r,-,?)).
:- rdf_meta(rdf_collection(r,-,?)).
:- rdf_meta(rdf_collection0(r,-,?)).
:- rdf_meta(rdf_collection_member(r,r,?)).
:- rdf_meta(rdf_collection_nth(?,r,r,?)).
:- rdf_meta(rdf_container_membership_property(r,?)).
:- rdf_meta(rdf_seq(r,-,?)).



%! rdf_alt(?Alt:uri, -Contents:list(uri), ?Graph:atom) is nondet.
% Alternative collections.
% No duplicates and unordered.

rdf_alt(Alt, Contents, G):-
  rdfs_individual_of(Alt, rdf:'Alt'),
  rdf_subject(Alt, G),
  rdf_collection_contents(Alt, Contents, G).


%! rdf_assert_collection_member(
%!   +Collection:or([bnode,iri]),
%!   +Member:or([bnode,iri]),
%!   +RdfGraph:atom
%! ) is det.

rdf_assert_collection_member(Collection, Member, G):-
  % Since it is too expensive to calculate the number of elements
  % in a collection each time a new element is added,
  % we store this number in a global variable.
  flag(Collection, MemberId, MemberId + 1),
  atomic_concat('_', MemberId, PName),
  rdf_global_id(rdf:PName, P),
  rdf_assert(Collection, P, Member, G).


%! rdf_bag(+Bag:uri, -Contents:list(uri), +Graph:atom) is nondet.
% Returns bags and their contents in the given graph.
% Unordered & duplicates allowed.
%
% @arg Bag An RDF bag.
% @arg Contents A list of resources.
% @arg Graph The atomic name of a graph.

rdf_bag(Bag, Contents, G):-
  rdfs_individual_of(Bag, rdf:'Bag'),
  rdf_subject(Bag, G),
  rdf_collection_contents(Bag, Contents, G).


rdf_collection(Collection, Contents, Graph):-
  nonvar(Collection), !,
  rdf_collection0(Collection, Contents, Graph), !.
rdf_collection(Collection, Contents, Graph):-
  rdf_collection0(Collection, Contents, Graph).

rdf_collection0(Collection, Contents, G):-
  rdf_alt(Collection, Contents, G).
rdf_collection0(Collection, Contents, G):-
  rdf_bag(Collection, Contents, G).
rdf_collection0(Collection, Contents, G):-
  rdf_seq(Collection, Contents, G).


%! rdf_collection_contents(
%!   ?Collection:uri,
%!   ?Contents:list(uri),
%!   ?Graph:atom
%! ) is nondet.

rdf_collection_contents(Collection, Contents, G):-
  findall(
    I-Content,
    (
      rdf(Collection, ContainerMembershipProperty, Content, G),
      rdf_container_membership_property(ContainerMembershipProperty, I)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, Contents).


%! rdf_collection_member(
%!   ?Member:or([bnode,iri]),
%!   ?Collection:or([bnode,iri]),
%!   ?Graph
%! ) is nondet.

rdf_collection_member(Member, Collection, Graph):-
  rdf_collection_nth(_, Collection, Member, Graph).


%! rdf_collection_nth(
%!   ?Index:positive_integer,
%!   ?Collection:iri,
%!   ?Member:iri,
%!   ?Graph:atom
%! ) is nondet.

rdf_collection_nth(I, Collection, Member, Graph):-
  maplist(var, [Collection,Member]), !,
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
% Succeeds if =Predicate= is a container membership property with =Index=.

% (+,?)
rdf_container_membership_property(P, Index):-
  must_be(nonvar, P), !,
  rdf_global_id(rdf:LocalName, P),
  atom_concat('_', Atom, LocalName),
  atom_number(Atom, Index),
  must_be(nonneg, Index).
% (?,+)
rdf_container_membership_property(P, Index):-
  is_of_type(positive_integer, Index), !,
  atom_number(Atom, Index),
  atom_concat('_', Atom, LocalName),
  rdf_global_id(rdf:LocalName, P).
% (?,?)
rdf_container_membership_property(P, Index):-
  between(1, inf, Index),
  rdf_container_membership_property(P, Index).


%! rdf_seq(
%!   ?Sequence:or([bnode,iri]),
%!   -Contents:list(or([bnode,iri,literal])),
%!   ?Graph:atom
%! ) is nondet.
% Ordered.

rdf_seq(Seq, Contents, G):-
  rdfs_individual_of(Seq, rdf:'Seq'),
  rdf_subject(Seq, G),
  rdf_collection_contents(Seq, Contents, G).

