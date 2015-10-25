:- module(
  rdf_container,
  [
    rdf_alt/2, % ?Alt, ?Contents
    rdf_alt/3, % ?Alt:rdf_term
               % ?Contents:list
               % ?Graph:atom
    rdf_alt_raw/2, % ?Alt, ?Contents
    rdf_alt_raw/3, % ?Alt:rdf_term
                   % -Contents:list
                   % ?Graph:atom
    rdf_assert_alt/2, % +List, ?Alt
    rdf_assert_alt/3, % +List:list
                      % ?Alt:rdf_term
                      % ?Graph:atom
    rdf_assert_bag/2, % +List, ?Bag
    rdf_assert_bag/3, % +List:list
                      % ?Bag:rdf_term
                      % ?Graph:atom
    rdf_assert_seq/2, % +List, ?Seq
    rdf_assert_seq/3, % +List:list
                      % ?Seq:rdf_term
                      % ?Graph:atom
    rdf_bag/2, % ?Bag, ?Content
    rdf_bag/3, % ?Bag:rdf_term
               % ?Contents:list
               % ?Graph:atom
    rdf_bag_raw/2, % ?Bag, ?Content
    rdf_bag_raw/3, % ?Bag:rdf_term
                   % ?Contents:list
                   % ?Graph:atom
    rdf_container_membership_property/2, % +Predicate:iri
                                         % -Index:positive_integer
    rdf_seq/2, % ?Seq, ?Contents
    rdf_seq/3 % ?Seq:rdf_term
              % ?Contents:list
              % ?Graph:atom
  ]
).

/** <module> RDF container

Support for RDF containers (sequence, bag, and alternatives).

@author Wouter Beek
@tbd Add predicates for building containers.
@version 2015/08
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(rdfs/rdfs_read)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta(rdf_alt(o,?)).
:- rdf_meta(rdf_alt(o,?,?)).
:- rdf_meta(rdf_alt_raw(o,t)).
:- rdf_meta(rdf_alt_raw(o,t,?)).
:- rdf_meta(rdf_assert_alt(t,r)).
:- rdf_meta(rdf_assert_alt(t,r,?)).
:- rdf_meta(rdf_assert_bag(t,r)).
:- rdf_meta(rdf_assert_bag(t,r,?)).
:- rdf_meta(rdf_assert_collection0(t,+,r,?)).
:- rdf_meta(rdf_assert_seq(t,r)).
:- rdf_meta(rdf_assert_seq(t,r,?)).
:- rdf_meta(rdf_bag(o,?)).
:- rdf_meta(rdf_bag(o,?,?)).
:- rdf_meta(rdf_bag_raw(o,t)).
:- rdf_meta(rdf_bag_raw(o,t,?)).
:- rdf_meta(rdf_container_membership_property(r,?)).
:- rdf_meta(rdf_seq(o,?)).
:- rdf_meta(rdf_seq(o,?,?)).
:- rdf_meta(rdf_seq_raw(o,t)).
:- rdf_meta(rdf_seq_raw(o,t,?)).





%! rdf_alt(?Alt:rdf_term, ?Contents:list) is nondet.

rdf_alt(L1, L2):-
  rdf_alt(L1, L2, _).


%! rdf_alt(?Alt:rdf_term, ?Contents:list, ?Graph:atom) is nondet.

rdf_alt(L1, L2, G):-
  rdf_alt_raw(L1, L0, G),
  maplist(rdf_interpreted_term, L0, L2).


%! rdf_alt_raw(?Alt:rdf_term, ?Contents:list(rdf_term)) is nondet.

rdf_alt_raw(L1, L2):-
  rdf_alt_raw(L1, L2, _).


%! rdf_alt_raw(
%!   ?Alt:rdf_term,
%!   ?Contents:list(rdf_term),
%!   ?Graph:atom
%! ) is nondet.
% RDF alternative collections.
% No duplicates and unordered.

rdf_alt_raw(L1, L2, G):-
  rdfs_instance(L1, rdf:'Alt'),
  % Satisfy graph restriction, if present.
  (ground(G) -> once(rdf_term(L1, G)) ; true),
  rdf_collection0(L1, L2, G).



%! rdf_assert_alt(+List:list, ?Alt:rdf_term) is det.

rdf_assert_alt(L1, L2):-
  rdf_assert_alt(L1, L2, _).

%! rdf_assert_alt(+List:list, ?Alt:rdf_term, ?Graph:atom) is det.

rdf_assert_alt(L1, L2, G):-
  rdf_assert_collection0(rdf:'Alt', L1, L2, G).



%! rdf_assert_bag(+List:list, ?Bag:rdf_term) is det.

rdf_assert_bag(L1, L2):-
  rdf_assert_bag(L1, L2, _).

%! rdf_assert_bag(+List:list, ?Bag:rdf_term, ?Graph:atom) is det.

rdf_assert_bag(L1, L2, G):-
  rdf_assert_collection0(rdf:'Bag', L1, L2, G).



%! rdf_assert_seq(+List:list, ?Seq:rdf_term) is det.

rdf_assert_seq(L1, L2):-
  rdf_assert_seq(L1, L2, _).

%! rdf_assert_alt(+List:list, ?Seq:rdf_term, ?Graph:atom) is det.

rdf_assert_seq(L1, L2, G):-
  rdf_assert_collection0(rdf:'Seq', L1, L2, G).



%! rdf_bag(?Bag:rdf_term, ?Contents:list) is nondet.

rdf_bag(L1, L2):-
  rdf_bag(L1, L2, _).


%! rdf_bag(?Bag:rdf_term, ?Contents:list, ?Graph:atom) is nondet.

rdf_bag(L1, L2, G):-
  rdf_bag_raw(L1, L0, G),
  maplist(rdf_interpreted_term, L0, L2).


%! rdf_bag_raw(?Bag:rdf_term, ?Contents:list(rdf_term)) is nondet.
% Returns bags and their contents in the given graph.
% No duplicates and unordered.

rdf_bag_raw(L1, L2):-
  rdf_bag_raw(L1, L2, _).


%! rdf_bag_raw(
%!   ?Bag:rdf_term,
%!   ?Contents:list(rdf_term),
%!   ?Graph:atom
%! ) is nondet.
% RDF alternative collections.
% Duplicates and ordered.

rdf_bag_raw(L1, L2, G):-
  rdfs_instance(L1, rdf:'Seq'),
  % Satisfy graph restriction, if present.
  (ground(G) -> once(rdf_term(L1, G)) ; true),
  rdf_collection0(L1, L2, G).



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
rdf_container_membership_property(P, N):-
  ground(P), !,
  rdf_global_id(rdf:Local, P),
  atom_concat('_', N0, Local),
  atom_number(N0, N),
  must_be(nonneg, N).
% (?,+)
rdf_container_membership_property(P, N):-
  ground(N), !,
  must_be(positive_integer, N),
  atom_number(N0, N),
  atom_concat('_', N0, Local),
  rdf_global_id(rdf:Local, P).
% (?,?)
rdf_container_membership_property(P, N):-
  between(1, inf, N),
  rdf_container_membership_property(P, N).



%! rdf_seq(?Seq:rdf_term, ?Contents:list) is nondet.

rdf_seq(L1, L2):-
  rdf_seq(L1, L2, _).


%! rdf_seq(?Seq:rdf_term, ?Contents:list, ?Graph:atom) is nondet.

rdf_seq(L1, L2, G):-
  rdf_seq_raw(L1, L0, G),
  maplist(rdf_interpreted_term, L0, L2).


%! rdf_seq_raw(?Seq:rdf_term, ?Contents:list(rdf_term)) is nondet.

rdf_seq_raw(L1, L2):-
  rdf_seq_raw(L1, L2, _).


%! rdf_seq_raw(
%!   ?Seq:rdf_term,
%!   ?Contents:list(rdf_term),
%!   ?Graph:atom
%! ) is nondet.
% RDF alternative collections.
% Duplicates and ordered.

rdf_seq_raw(L1, L2, G):-
  rdfs_instance(L1, rdf:'Seq'),
  % Satisfy graph restriction, if present.
  (ground(G) -> once(rdf_term(L1, G)) ; true),
  rdf_collection0(L1, L2, G).





% HELPERS %

rdf_assert_collection0(C, L1, L2, G):-
  rdf_transaction(rdf_assert_collection0(1, C, L1, L2, G)).

rdf_assert_collection0(N, C, L1, L2, G):-
  (var(L2) -> rdf_bnode(L2) ; true),
  rdf_assert_instance(L2, C, G),
  rdf_assert_collection_items0(N, L1, L2, G).

rdf_assert_collection_items0(_, [], _, _):- !.
rdf_assert_collection_items0(N1, [H1|T1], L2, G):-
  rdf_container_membership_property(P, N1),
  user:rdf_assert(L2, P, H1, G),
  N2 is N1 + 1,
  rdf_assert_collection_items0(N2, T1, L2, G).



rdf_collection0(L1, L2, G):-
  aggregate_all(
    set(I-X),
    (
      user:rdf(L1, P, X, G),
      rdf_container_membership_property(P, I)
    ),
    Pairs
  ),
  pairs_values(Pairs, L2).
