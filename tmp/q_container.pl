:- module(
  rdf_container,
  [
    rdf_container/4,       % +M, +S, -L, ?G
    rdf_container_member/5 % +M, ?S, ?N, ?X, ?G
  ]
).

/** <module> Quine container

@author Wouter Beek
@version 2016/09
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(rdf/rdf_api)).

:- rdf_meta
   rdf_container(+, r, -, r),
   rdf_container_member(+, r, ?, r, r).





%! rdf_container(+M, +S, -L, ?G) is nondet.
%
% True when List is the list of objects attached to Container using a
% container membership property (rdf:_0, rdf:_1, ...). If multiple
% objects are connected to the Container using the same membership
% property, this predicate selects one value non-deterministically.

rdf_container(M, S, L, G) :-
  rdf_subject(M, S, G),
  findall(N-X, rdf_container_member(M, S, N, X, G), Pairs),
  group_pairs_by_key(Pairs, Groups),
  asc_pairs_values(Groups, AscLs),
  maplist(member, L, AscLs).



%! rdf_container_member(+M, ?S, ?N, ?X, ?G) is nondet.
%
% What is the most efficient way to enumerate `rdfs_member(-,-)`?
%
% 1. If we enumerate over all container membership properties (= the
%    current implementation) then it takes N steps before we get to
%    triple `〈Container, rdf:_N, Elem〉`, for arbitrary N.
%
% 2. The alternative is to enumerate over all triples and check
%    whether the predicate term is a container membership property.
%
% 3. The choice between (1) and (2) depends on whether the number of
%    currently loaded triples in larger/smaller than the largest
%    number that appears in a container membership property.  This
%    means enumerating over all predicate terms using rdf_predicate/1.

rdf_container_member(M, S, N, X, G) :-
  (nonvar(S) ; nonvar(X)), !,
  t(M, S, P, X, G),
  rdf_container_membership_property(P, N).
rdf_container_member(M, S, N, X, G) :-
  rdf_container_membership_property(P, N),
  t(M, S, P, X, G).



%! rdf_container_membership_property(?P, ?N) is nondet.
%
% True when Property is the Nth container membership property.
%
% Success of this goal does not imply that Property is present in the
% database.

rdf_container_membership_property(P, N) :-
  var(P), !,
  between(1, inf, N),
  rdf_equal(rdf:'_', Prefix),
  atom_concat(Prefix, N, P).
rdf_container_membership_property(P, N) :-
  rdf_equal(rdf:'_', Prefix),
  string_concat(Prefix, NumS, P),
  number_string(N, NumS),
  integer(N),
  N >= 0.
