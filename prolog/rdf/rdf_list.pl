:- module(
  rdf_list,
  [
    rdf_assert_list/2, % +PrologList:list
                       % ?RdfList:or([bnode,iri])
    rdf_assert_list/3 % +PrologList:list
                      % ?RdfList:or([bnode,iri])
                      % ?Graph:atom
  ]
).

/** <module> RDF list

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(rdf/rdf_build)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_assert_list(r,r)).
:- rdf_meta(rdf_assert_list(r,r,?)).





%! rdf_assert_list(+PrologList:list, ?RdfList:or([bnode,iri])) is det.

rdf_assert_list(L1, L2):-
  rdf_assert_list(L1, L2, _).

%! rdf_assert_list(
%!   +PrologList:list,
%!   ?RdfList:or([bnode,iri]),
%!   ?Graph:atom
%! ) is det.
% Asserts the given, possibly nested, list into RDF.

rdf_assert_list([], rdf:nil, _):- !.
rdf_assert_list(L1, L2, G):-
  add_list_instance(L2, G),
  rdf_assert_list_items(L1, L2, G).

rdf_assert_list_items([], rdf:nil, _).
rdf_assert_list_items([H1|T1], L2, G):-
  % rdf:first
  (   % Nested list.
      is_list(H1)
  ->  rdf_assert_list(H1, H2, G)
  ;   % Non-nested list.
      H2 = H1
  ),
  rdf_assert2(L2, rdf:first, H2, G),

  % rdf:rest
  (   T1 == []
  ->  rdf_global_id(rdf:nil, T2)
  ;   add_list_instance(T2, G),
      rdf_assert_list_items(T1, T2, G)
  ),
  rdf_assert2(L2, rdf:rest, T2, G).

add_list_instance(L, G):-
  (   var(L)
  ->  rdf_bnode(L)
  ;   true
  ),
  rdf_assert_instance(L, rdf:'List', G).
