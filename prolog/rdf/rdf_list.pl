:- module(
  rdf_list,
  [
    rdf_assert_list/2, % +PrologList:list
                       % ?RdfList:or([bnode,iri])
    rdf_assert_list/3, % +PrologList:list
                       % ?Datatype:iri
                       % ?RdfList:or([bnode,iri])
    rdf_assert_list/4, % +PrologList:list
                       % ?Datatype:iri
                       % ?RdfList:or([bnode,iri])
                       % ?Graph:atom
    rdf_list/2, % +PrologList:list
                % ?RdfList:or([bnode,iri])
    rdf_list/3 % +PrologList:list
               % ?RdfList:or([bnode,iri])
               % ?Graph:atom
  ]
).

/** <module> RDF list

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_meta(rdf_assert_list(+,r)).
:- rdf_meta(rdf_assert_list(+,r,r)).
:- rdf_meta(rdf_assert_list(+,r,r,?)).
:- rdf_meta(rdf_is_list(r)).
:- rdf_meta(rdf_list(r,-)).
:- rdf_meta(rdf_list(r,-,?)).





%! rdf_assert_list(+PrologList:list, ?RdfList:or([bnode,iri])) is det.

rdf_assert_list(L1, L2):-
  rdf_assert_list(L1, _, L2).

%! rdf_assert_list(
%!   +PrologList:list,
%!   ?Datatype:iri,
%!   ?RdfList:or([bnode,iri])
%! ) is det.

rdf_assert_list(L1, D, L2):-
  rdf_assert_list(L1, D, L2, _).

%! rdf_assert_list(
%!   +PrologList:list,
%!   ?Datatype:iri,
%!   ?RdfList:or([bnode,iri]),
%!   ?Graph:atom
%! ) is det.
% Asserts the given, possibly nested, list into RDF.

rdf_assert_list(L1, D, L2, G):-
  rdf_transaction(rdf_assert_list0(L1, D, L2, G)).

rdf_assert_list0([], _, rdf:nil, _):- !.
rdf_assert_list0(L1, D, L2, G):-
  add_list_instance0(L2, G),
  rdf_assert_list_items0(L1, D, L2, G).

rdf_assert_list_items0([], _, rdf:nil, _).
rdf_assert_list_items0([H1|T1], D, L2, G):-
  % rdf:first
  (   % Nested list.
      is_list(H1)
  ->  rdf_assert_list0(H1, D, H2, G)
  ;   % Non-nested list.
      H2 = H1
  ),
  (   nonvar(D)
  ->  rdf_assert_literal(L2, rdf:first, D, H2, G)
  ;   rdf_assert2(L2, rdf:first, H2, G)
  ),

  % rdf:rest
  (   T1 == []
  ->  rdf_global_id(rdf:nil, T2)
  ;   add_list_instance0(T2, G),
      rdf_assert_list_items0(T1, D, T2, G)
  ),
  rdf_assert2(L2, rdf:rest, T2, G).

add_list_instance0(L, G):-
  (   var(L)
  ->  rdf_bnode(L)
  ;   true
  ),
  rdf_assert_instance(L, rdf:'List', G).



%! rdf_is_list(@Term) is semidet.

rdf_is_list(L):-
  rdfs_individual_of(L, rdf:'List').



%! rdf_list(+RdfList:or([bnode,iri]), +PrologList:list) is semidet.
% @see rdf_list/3

rdf_list(L1, L2):-
  rdf_list(L1, L2, _).

%! rdf_list(
%!   +RdfList:or([bnode,iri]),
%!   ?PrologList:list,
%!   ?Graph:atom
%! ) is semidet.

rdf_list(rdf:nil, [], _):- !.
rdf_list(L1, [H2|T2], G):-
  % rdf:first
  rdf2(L1, rdf:first, H1, G),
  (   % Nested list
      rdf_is_list(H1)
  ->  rdf_list(H1, H2, G)
  ;   % Non-nested list.
      H2 = H1
  ),
  % rdf:rest
  rdf2(L1, rdf:rest, T1, G),
  rdf_list(T1, T2, G).
