:- module(
  rdf_build,
  [
  % RDF
    rdf_assert/2,              % +M, +Quad
    rdf_assert/3,              % +M, +Triple, +G
    rdf_assert/4,              % +M, +S, +P, +O
    rdf_assert/5,              % +M, +S, +P, +O, +G
   %rdf_create_alias/2,        % +Alias, +Prefix
   %rdf_create_bnode/1         % -BNode
    rdf_create_bnode_prefix/1, % -BPrefix
    rdf_assert_instance/4,     % +M, +I, ?C, +G
    rdf_assert_instances/4,    % +M, +I, +Cs, +G
    rdf_assert_list/4,         % +M, +L1, -L2, +G
    rdf_assert_list/5,         % +M, +S,  +P,  +L, +G
    rdf_assert_now/4,          % +M, +S, +P, +G
    rdf_assert_now/5,          % +M, +S, +P, +D, +G
    rdf_assert_objects/5,      % +M, +S, +P, +Os, +G
    rdf_assert_reification/4,  % +M, +Triple, +G, ?Stmt
    rdf_assert_rev/5,          % +M, +O, +P, +S, +G
  % DOMAIN-SPECIFIC
    rdf_assert_action/5, % +M, +ActionC, +Actor, -Action, +G
    rdf_assery_mail/4,   % +M, +Agent, +Mail, +G
    rdf_assert_user/7    % +M, +User, +C, +Img, +GivenName, +FamilyName, +G
  ]
).
:- reexport(library(semweb/rdf11), [
     rdf_create_bnode/1,
     rdf_register_prefix/2 as rdf_create_alias
   ]).

/** <module> RDF build API

@author Wouter Beek
@version 2016/06-2017/01
*/

:- use_module(library(date_time/date_time)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_list)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(uri)).
:- use_module(library(uuid)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- rdf_meta
   % RDF
   rdf_assert(+, t),
   rdf_assert(+, t, r),
   rdf_assert(+, r, r, o),
   rdf_assert(+, r, r, o, r),
   rdf_assert_instance(+, r, r, r),
   rdf_assert_instances(+, r, t, r),
   rdf_assert_list(+, t, -, r),
   rdf_assert_list(+, r, r, t, r),
   rdf_assert_now(+, o, r, r),
   rdf_assert_now(+, o, r, r, r),
   rdf_assert_objects(+, r, r, t, r),
   rdf_assert_reification(+, t, r, r),
   rdf_assert_rev(+, o, r, r, r),
   rdf_retract(+, t),
   rdf_retract(+, t, r),
   rdf_retract(+, r, r, o, r),
   % DOMAIN-SPECIFIC
   rdf_assert_action(+, r, r, -, r),
   rdf_assert_mail(+, r, +, r),
   rdf_assert_user(+, r, r, +, +, +, r).





%! rdf_assert(+M, +Quad) is det.
%! rdf_assert(+M, +Triple, +G) is det.
%! rdf_assert(+M, +S, +P, +O) is det.
%! rdf_assert(+M, +S, +P, +O, +G) is det.
%
% @tbd HDT support.

rdf_assert(M, rdf(S,P,O,G)) :-
  rdf_assert(M, S, P, O, G).


rdf_assert(M, rdf(S,P,O), G) :-
  rdf_assert(M, S, P, O, G).


rdf_assert(stream(State,Out), S, P, O) :- !,
  rdf_write_ntuple(S, P, O, State, Out).
rdf_assert(trp, S, P, O, G) :- !,
  rdf_assert(S, P, O, G).



%! rdf_create_bnode_prefix(-BNodePrefix) is det.
%
% Creates a universally unique blank node label prefix.

rdf_create_bnode_prefix(BNodePrefix) :-
  uuid(Uuid),
  atomic_list_concat(['_',Uuid,''], :, BNodePrefix).



%! rdf_assert_instance(+M, +I, ?C, +G) is det.

rdf_assert_instance(M, I, C, G) :-
  rdf_alias_defval(C, rdfs:'Resource'),
  rdf_assert(M, I, rdf:type, C, G).



%! rdf_assert_instances(+M, +I, +Cs, +G) is det.

rdf_assert_instances(M, I, Cs, G) :-
  maplist({M,I,G}/[C]>>rdf_assert_instance(M, I, C, G), Cs).



%! rdf_assert_list(+M, +L1, -L2, +G) is det.
%! rdf_assert_list(+M, +S, +P, +L, +G) is det.

rdf_assert_list(M, L1, L2, G) :-
  rdf_transaction(rdf_assert_list0(M, L1, L2, G)).


rdf_assert_list0(_, [], L, _) :-
  % @tbd RDF prefix expansion does not work.
  rdf_equal(rdf:nil, L).
rdf_assert_list0(M, [H|T], L, G) :-
  (var(L) -> rdf_create_bnode(L) ; true),
  % @tbd RDF alias expansion does not work.
  rdf_equal(rdf:'List', C),
  rdf_assert(M, L, rdf:type, C, G),
  rdf_assert(M, L, rdf:first, H, G),
  (   T == []
  ->  rdf_equal(rdf:nil, I),
      rdf_assert(M, L, rdf:rest, I, G)
  ;   rdf_create_bnode(T2),
      rdf_assert(M, L, rdf:rest, T2, G),
      rdf_assert_list0(M, T, T2, G)
  ).


rdf_assert_list(M, S, P, L1, G) :-
  rdf_transaction((
    rdf_assert_list0(M, L1, L2, G),
    rdf_assert(M, S, P, L2, G)
  )).



%! rdf_assert_now(+M, +S, +P, +G) is det.
%! rdf_assert_now(+M, +S, +P, +D, +G) is det.
%
% The default date/time datatype is `xsd:dateTime`.

rdf_assert_now(M, S, P, G) :-
  rdf_assert_now(M, S, P, xsd:dateTime, G).


rdf_assert_now(M, S, P, D, G) :-
  get_time(Now),
  'something_to_dt-rdf'(Now, D, Val),
  rdf_assert(M, S, P, Val^^D, G).



%! rdf_assert_objects(+M, +S, +P, +Os, +G) is det.

rdf_assert_objects(M, S, P, Os, G) :-
  maplist({M,S,P,G}/[O]>>rdf_assert(M, S, P, O, G), Os).



%! rdf_assert_reification(+M, +Triple, +G, ?Stmt) is det.
%
% If Stmt is not given a fresh blank node is created.

rdf_assert_reification(M, rdf(S,P,O), G, Stmt) :-
  defgoal(rdf_create_bnode, Stmt),
  rdf_assert_instance(M, Stmt, rdf:'Stmt', G),
  rdf_assert(M, Stmt, rdf:subject, S, G),
  rdf_assert(M, Stmt, rdf:predicate, P, G),
  rdf_assert(M, Stmt, rdf:object, O, G),
  rdf_assert(M, S, P, O, G).



%! rdf_assert_rev(+M, +O, +P, +S, +G) is det.

rdf_assert_rev(M, O, P, S, G) :-
  rdf_assert(M, S, P, O, G).



%! rdf_retract(+M, +Quad) is det.
%! rdf_retract(+M, +Triple, +G) is det.
%! rdf_retract(+M, +S, +P, +O, +G) is det.

rdf_retract(M, rdf(S,P,O,G)) :-
  rdf_retract(M, S, P, O, G).


rdf_retract(M, rdf(S,P,O), G) :-
  rdf_retract(M, S, P, O, G).


rdf_retract(trp, S, P, O, G) :- !,
  rdf11:rdf_retractall(S, P, O, G).





% DOMAIN-SPECIFIC %

%! rdf_assert_action(+M, +ActionC, +Actor, -Action, +G) is det.

rdf_assert_action(M, ActionC, Actor, Action, G):-
  rdf_tbox_iri(nsdef, [action], Action),
  rdf_assert_instance(M, Action, ActionC, G),
  rdf_assert_now(M, Action, prov:atTime, G),
  rdf_assert(M, Action, prov:wasAssociatedWith, Actor, G).



%! rdf_assert_mail(+M, +Agent, +Mail, +G) is det.

rdf_assert_mail(M, Agent, Mail, G) :-
  atomic_list_concat([mailto,Mail], :, Iri),
  rdf_assert(M, Agent, foaf:mbox, Iri^^xsd:anyURI, G).



%! rdf_assert_user(+M, +User, +C, +Img, +GivenName, +FamilyName, +G) is det.

rdf_assert_user(M, User, C, Img, GivenName, FamilyName, G) :-
  rdf_assert_instance(M, User, C, G),
  rdf_assert(M, User, foaf:depiction, Img^^xsd:anyURI, G),
  rdf_assert(M, User, foaf:familyName, FamilyName@nl, G),
  rdf_assert(M, User, foaf:givenName, GivenName@nl, G).
