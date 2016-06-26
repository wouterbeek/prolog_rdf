:- module(
  vocab_ext,
  [
    rdf_assert_action/4,    % +ActionC, +Actor, -Action, +G
    rdf_assert_instance/2,  % +I, ?C
    rdf_assert_instance/3,  % +I, ?C, +G
    rdf_assert_instances/2, % +I, +Cs
    rdf_assert_instances/3, % +I, +Cs, +G
    rdf_assert_list/4,      % +S, +P, +L, +G
    rdf_assert_now/2,       % +S, +P
    rdf_assert_now/3,       % +S, +P, +D
    rdf_assert_now/4,       % +S, +P, +D, +G
    rdf_assert_objects/3,   % +S, +P, +Os
    rdf_assert_objects/4,   % +S, +P, +Os, +G
    rdf_assert_rev/3,       % +O, +P, +S
    rdf_assert_rev/4        % +O, +P, +S, +G
  ]
).

/** <module> Vocabulary assertions

@author Wouter Beek
@version 2016/06
*/

:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_ext)).
:- use_module(library(semweb/rdf11)).

:- rdf_register_prefix(prov, 'http://www.w3.org/ns/prov#').

:- rdf_meta
  rdf_assert_action(r, r, -, r).





%! rdf_assert_action(+ActionC, +Actor, -Action, +G) is det.

rdf_assert_action(ActionC, Actor, Action, G):-
  rdf_create_iri(vzm, [action], Action),
  rdf_assert_instance(Action, ActionC, G),
  rdf_assert_now(Action, prov:atTime, G),
  rdf_assert(Action, prov:wasAssociatedWith, Actor, G).



%! rdf_assert_instance(+I, ?C) is det.
%! rdf_assert_instance(+I, ?C, ?G) is det.

rdf_assert_instance(I, C) :-
  rdf_assert_instance(I, C, _).


rdf_assert_instance(I, C, G) :-
  rdf_defval(C, rdfs:'Resource'),
  rdf_assert(I, rdf:type, C, G).



%! rdf_assert_instances(+I, +Cs) is det.
%! rdf_assert_instances(+I, +Cs, ?G) is det.

rdf_assert_instances(I, Cs) :-
  rdf_assert_instances(I, Cs, _).

rdf_assert_instances(I, Cs, G) :-
  maplist({I,G}/[C]>>rdf_assert_instance(I, C, G), Cs).



%! rdf_assert_list(+S, +P, +L, +G) is det.

rdf_assert_list(S, P, L, G) :-
  rdf_assert_list(L, B, G),
  rdf_assert(S, P, B, G).



%! rdf_assert_now(+S, +P) is det.
%! rdf_assert_now(+S, +P, +D) is det.
%! rdf_assert_now(+S, +P, +D, +G) is det.

rdf_assert_now(S, P) :-
  rdf_assert_now(S, P, xsd:dateTime).

rdf_assert_now(S, P, D) :-
  rdf_assert_now(S, P, D, default).

rdf_assert_now(S, P, D, G) :-
  get_time(Now),
  rdf_assert(S, P, Now^^D, G).



%! rdf_assert_objects(+S, +P, +Os) is det.
%! rdf_assert_objects(+S, +P, +Os, +G) is det.

rdf_assert_objects(S, P, Os) :-
  rdf_default_graph(G),
  rdf_assert_objects(S, P, Os, G).


rdf_assert_objects(S, P, Os, G) :-
  forall(member(O, Os), rdf_assert(S, P, O, G)).



%! rdf_assert_rev(+O, +P, +S) is det.

rdf_assert_rev(O, P, S) :-
  rdf_assert_rev(S, P, O).


%! rdf_assert_rev(+O, +P, +S, +G) is det.

rdf_assert_rev(O, P, S, G) :-
  rdf_assert(S, P, O, G).
