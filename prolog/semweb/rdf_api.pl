:- encoding(utf8).
:- module(
  rdf_api,
  [
    assert_instance/3,               % +Backend, +I, +C
    assert_list/3,                   % +Backend, +PrologList, -RdfList
    assert_list_triple/4,            % +Backend, +S, +P, +PrologList
    assert_now/3,                    % +Backend, +S, +P
    assert_now/4,                    % +Backend, +S, +P, +D
    assert_reification/5,            % +Backend, +S, +P, +O, -Statement
    assert_reification_triple/5,     % +Backend, +S, +P, +O, -Statement
    assert_shape/3,                  % +Backend, +Feature, +Shape
    assert_shape/4,                  % +Backend, +Feature, +Shape, -Geometry
    assert_triple/4,                 % +Backend, +S, +P, +O
    container_member/3,              % +Backend, ?Member, ?Container
    container_nth1/4,                % +Backend, ?N, ?Container, ?Member
    container_membership_property/3, % ?Backend, ?P, ?N
    instance/3,                      % ?Backend, ?I, ?C
    list/3,                          % ?Backend, +RdfList, -PrologList
    list_compare/4,                  % +Backend, +X, +Y, -Order
    list_last/3,                     % ?Backend, ?RdfList, ?X
    list_last_tp/4,                  % ?Backend, ?S, ?P, ?X
    list_member/3,                   % ?Backend, ?X, ?RdfList
    list_member_tp/4,                % ?Backend, ?S, ?P, ?X
    list_tp/4,                       % ?Backend, ?S, ?P, -PrologList
    node/2,                          % ?Backend, ?Node
    predicate/2,                     % ?Backend, ?P
    reification/5,                   % ?Backend, ?S, ?P, ?O, -Statement
    strict_subclass/3,               % ?Backend, ?C, ?D
    strict_subproperty/3,            % ?Backend, ?P, ?Q
    subclass/3,                      % ?Backend, ?C, ?D
    subproperty/3,                   % ?Backend, ?P, ?Q
    term_string/3,                   % ?Backend, +Term, -String
    tp/4,                            % ?Backend, ?S, ?P, ?O
    tp_chk/4,                        % ?Backend, ?S, ?P, ?O
    tp_count/5,                      % ?Backend, ?S, ?P, ?O, -N
    tp_retractall/4,                 % +Backend, ?S, ?P, ?O
    tp_string/4,                     % ?Backend, ?S, ?P, -String
    tp_update/5,                     % +Backend, ?S, ?P, ?O, +Action
    tp_value/4                       % ?Backend, ?S, ?P, -Value
  ]
).

/** <module> RDF API

Backend-independent RDF API.

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).

:- use_module(library(closure)).
:- use_module(library(date_time)).
:- use_module(library(default)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(xsd/xsd)).

:- maplist(rdf_register_prefix, [rdf,rdfs]).

:- multifile
    rdf_api:assert_triple_/4,
    rdf_api:predicate_/2,
    rdf_api:tp_/4,
    rdf_api:tp_count_/5,
    rdf_api:tp_retractall_/4.

:- rdf_meta
   assert_instance(t, r, r),
   assert_list(t, t, r),
   assert_list_triple(t, r, r, t),
   assert_now(t, r, r),
   assert_now(t, r, r, r),
   assert_reification(t, r, r, o, r, r),
   assert_reification(t, r, r, o, r, r),
   assert_shape(t, r, +),
   assert_shape(t, r, +, -),
   assert_triple(t, r, r, o),
   container_member(t, r, r),
   container_nth1(t, ?, r, r),
   container_membership_propertry(t, r, ?),
   instance(t, r, o),
   list(t, r, -),
   list_compare(t, o, o, -),
   list_last(t, r, t),
   list_last_tp(t, r, r, o),
   list_member(t, o, r),
   list_member_tp(t, r, r, o),
   list_tp(t, r, r, -),
   node(t, o),
   predicate(t, r),
   reification(t, r, r, o, -),
   strict_subclass(t, r, r),
   strict_subproperty(t, r, r),
   subclass(t, r, r),
   subproperty(t, r, r),
   term_string(t, o, -),
   tp(t, r, r, o),
   tp_chk(t, r, r, o),
   tp_count(t, r, r, o, -),
   tp_retractall(t, r, r, o),
   tp_string(t, r, r, -),
   tp_update(t, r, r, o, t),
   tp_value(t, r, r, -).





%! assert_instance(+Backend, +Instance:rdf_subject, +Class:rdf_subject) is det.

assert_instance(B, I, C) :-
  assert_triple(B, I, rdf:type, C).



%! assert_list(+Backend, +PrologList:list(term), -RdfList:rdf_subject) is det.

assert_list(_, [], rdf:nil) :- !.
assert_list(B, [H1|T1], L2) :-
  call_default_value(L2, rdf_bnode_iri),
  assert_triple(B, L2, rdf:first, H1),
  (   T1 == []
  ->  rdf_equal(rdf:nil, T2),
      assert_triple(B, L2, rdf:rest, T2)
  ;   rdf_bnode_iri(T2),
      assert_triple(B, L2, rdf:rest, T2),
      assert_list(B, T1, T2)
  ).



%! assert_list_triple(+Backend, +S:rdf_subject, +P:rdf_preficate, +L:list(term)) is det.

assert_list_triple(B, S, P, L1) :-
  assert_list(B, L1, L2),
  assert_triple(B, S, P, L2).



%! assert_now(+S, +P, +G) is det.
%! assert_now(+S, +P, +D, +G) is det.
%
% The default date/time datatype is `xsd:dateTime`.

assert_now(B, S, P) :-
  assert_now(B, S, P, xsd:dateTime).


assert_now(B, S, P, D) :-
  now(Dt),
  xsd_date_time(Dt, D, XsdDt),
  assert_triple(B, S, P, XsdDt).



%! assert_reification(+Backend, +S:rdf_subject, +P:rdf_predicate, +O:rdf_object, +Statement:rdf_subject) is nondet.

assert_reification(B, S, P, O, Stmt) :-
  assert_triple(B, Stmt, rdf:subject, S),
  assert_triple(B, Stmt, rdf:predicate, P),
  assert_triple(B, Stmt, rdf:object, O).



%! assert_reification_triple(+Backend, +S:rdf_subject, +P:rdf_predicate, +O:rdf_object, +Statement:rdf_subject) is det.
%
% If Statement is not given a fresh blank node is created.

assert_reification_triple(B, S, P, O, Stmt) :-
  call_default_value(Stmt, rdf_bnode_iri),
  assert_instance(B, Stmt, rdf:'Statement'),
  assert_triple(B, Stmt, rdf:subject, S),
  assert_triple(B, Stmt, rdf:predicate, P),
  assert_triple(B, Stmt, rdf:object, O),
  assert_triple(B, S, P, O).



%! assert_shape(+Backend, +Feature:rdf_node, +Shape:compound) is det.
%! assert_shape(+Backend, +Feature:rdf_node, +Shape:compound, -Geometry:rdf_node) is det.

assert_shape(B, Feature, Shape) :-
  assert_shape(B, Feature, Shape, _).


assert_shape(B, Feature, Shape, Geometry) :-
  rdf_bnode_iri(Geometry),
  assert_instance(B, Geometry, geo:'Geometry'),
  assert_triple(B, Feature, geo:hasGeometry, Geometry),
  assert_triple(B, Geometry, geo:asWKT, Shape).



%! assert_triple(+Backend, +S:rdf_subject, +P:rdf_predicate, +O:rdf_object) is nondet.

assert_triple(B1, S, P1, O1) :-
  rdf_predicate_dwim(P1, P2),
  rdf_object_dwim(O1, O2),
  pre_backend_(B1, B2),
  rdf_api:assert_triple_(B2, S, P2, O2),
  post_backend_(B1, B2).

rdf_api:assert_triple_(dummy, _, _, _) :-
  fail.



%! container_member(+Backend, ?Member:rdf_node, ?Container:rdf_node) is nondet.
%
% True when List is the list of objects attached to Container using a
% container membership property (rdf:_0, rdf:_1, ...). If multiple
% objects are connected to the Container using the same membership
% property, this predicate selects one value non-deterministically.

container_member(B, X, L) :-
  container_nth1(B, _, L, X).



%! container_nth1(+Backend, ?N:positive_integer, ?Container:rdf_node, ?Member:rdf_node) is nondet.
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

container_nth1(B, N, L, X) :-
  ground(N), !,
  container_membership_property(B, P, N),
  tp(B, L, P, X).
container_nth1(B, N, L, X) :-
  tp(B, L, P, X),
  container_membership_property(B, P, N).



%! container_membership_property(?Backend, ?P:rdf_predicate, ?N:nonneg) .
%
% Container membership properties that are present in Backend.

container_membership_property(B, P, N) :-
  rdf_equal(rdf:'_', Prefix),
  predicate(B, P),
  string_concat(Prefix, NumS, P),
  number_string(N, NumS),
  integer(N),
  N >= 0.



%! instance(?Backend, ?Instance:rdf_term, ?Class:rdf_term) is nondet.

instance(B, I, D) :-
  ground(I), !,
  tp(B, I, rdf:type, C),
  path_closure0(subclass_(B), C, D).
instance(B, I, D) :-
  ground(D), !,
  path_closure0(subclass_(B), C, D),
  tp(B, I, rdf:type, C).
instance(_, C, D) :-
  instantiation_error(args([C,D])).



%! list(?Backend, +RdfList:rdf_subject, -PrologList:list(rdf_term)) is nondet.

list(B, L1, L2) :-
  % Ensure this is an RDF list.
  tp_chk(B, L1, rdf:rest, _),
  findall(X, list_member(B, X, L1), L2).



%! list_compare(+Backend, +X:rdf_term, +Y:rdf_term, -Order:oneof([<,0,>])) is det.
%
% When X and Y are RDF terms in the same RDF list, Order is unified
% with their respective order:
%
%   - `<' when X appears before Y in the list.
%
%   - `0' when X and Y are the same list item.
%
%   - `>' when X apears after Y in the list.

list_compare(B, X, X, 0) :-
  tp_chk(B, _, rdf:first, X), !.
list_compare(B, X, Y, <) :-
  'rdf_list_<'(B, X, Y), !.
list_compare(B, X, Y, >) :-
  'rdf_list_>'(B, X, Y).

'rdf_list_<'(_, X, X) :- !.
'rdf_list_<'(B, X, Z) :-
  tp(B, L1, rdf:first, X),
  tp(B, L1, rdf:rest, L2),
  tp(B, L2, rdf:first, Y),
  'rdf_list_<'(B, Y, Z).

'rdf_list_>'(_, X, X) :- !.
'rdf_list_>'(B, X, Z) :-
  tp(B, L1, rdf:first, X),
  tp(B, L2, rdf:rest, L1),
  tp(B, L2, rdf:first, Y),
  'rdf_list_<'(B, Y, Z).



%! list_last(?Backend, ?L:rdf_list, ?X:rdf_term) is nondet.
%
% X is the last item in RDF list L.

list_last(B, L, X) :-
  tp(B, L, rdf:rest, T),
  (   rdf_equal(T, rdf:nil)
  ->  tp(B, L, rdf:first, X)
  ;   list_last(B, T, X)
  ).



%! list_last_tp(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?X:rdf_term) is nondet.

list_last_tp(B, S, P, X) :-
  list_tp(B, S, P, L),
  list_last(B, L, X).



%! list_member(?Backend, ?X:rdf_term, ?L:rdf_list) is nondet.

list_member(B, X, L) :-
  tp(B, L, rdf:first, X).
list_member(B, X, L) :-
  tp(B, L, rdf:rest, T),
  list_member(B, X, T).



%! list_member_tp(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?X:rdf_term) is nondet.

list_member_tp(B, S, P, X) :-
  ground(X), !,
  list_member(B, X, L),
  tp(B, S, P, L).
list_member_tp(B, S, P, X) :-
  tp(B, S, P, L),
  list_member(B, X, L).



%! list_tp(?Backend, ?S:rdf_subject, ?P:rdf_predicate, -PrologList:list(term)) is nondet.

list_tp(B, S, P, L2) :-
  tp(B, S, P, L1),
  list(B, L1, L2).



%! node(?Backend, ?Node:rdf_term) is nondet.

% subject node
node(B, S) :-
  tp(B, S, _, _).
% object node
node(B, O) :-
  tp(B, _, _, O).



%! predicate(+Backend, +P:rdf_predicate) is semidet.
%! predicate(+Backend, -P:rdf_predicate) is nondet.
%! predicate(-Backend, +P:rdf_predicate) is nondet.
%! predicate(-Backend, -P:rdf_predicate) is nondet.

predicate(B, P) :-
  rdf_api:predicate_(B, P).

rdf_api:predicate_(dummy, _) :-
  fail.



%! reification(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object, -Statement:iri) is nondet.

reification(B, S, P, O, Stmt) :-
  tp(B, Stmt, rdf:subject, S),
  tp(B, Stmt, rdf:predicate, P),
  tp(B, Stmt, rdf:object, O).



%! strict_subclass(?Backend, +C:rdf_class, +D:rdf_class) is semidet.
%! strict_subclass(?Backend, +C:rdf_class, -D:rdf_class) is nondet.
%! strict_subclass(?Backend, -C:rdf_class, +D:rdf_class) is nondet.

strict_subclass(_, C, D) :-
  maplist(var, [C,D]), !,
  instantiation_error(args([C,D])).
strict_subclass(B, C, D) :-
  path_closure(subclass_(B), C, D).



%! strict_subproperty(?Backend, +C:rdf_property, +D:rdf_property) is semidet.
%! strict_subproperty(?Backend, +C:rdf_property, -D:rdf_property) is nondet.
%! strict_subproperty(?Backend, -C:rdf_property, +D:rdf_property) is nondet.

strict_subproperty(_, P, Q) :-
  maplist(var, [P,Q]), !,
  instantiation_error(args([P,Q])).
strict_subproperty(B, P, Q) :-
  path_closure(subproperty_(B), P, Q).



%! subclass(?Backend, +C:rdf_class, +D:rdf_class) is semidet.
%! subclass(?Backend, +C:rdf_class, -D:rdf_class) is multi.
%! subclass(?Backend, -C:rdf_class, +D:rdf_class) is multi.

subclass(_, C, D) :-
  maplist(var, [C,D]), !,
  instantiation_error(args([C,D])).
subclass(B, C, D) :-
  path_closure0(subclass_(B), C, D).

subclass_(B, C, D) :-
  tp(B, C, rdfs:subClassOf, D).



%! subproperty(?Backend, +P:rdf_property, +Q:rdf_property) is semidet.
%! subproperty(?Backend, +P:rdf_property, -Q:rdf_property) is multi.
%! subproperty(?Backend, -P:rdf_property, +Q:rdf_property) is multi.

subproperty(_, P, Q) :-
  maplist(var, [P,Q]), !,
  instantiation_error(args([P,Q])).
subproperty(B, P, Q) :-
  path_closure0(subproperty_(B), P, Q).

subproperty_(B, P, Q) :-
  tp(B, P, rdfs:subPropertyOf, Q).



%! term_string(+Backend, +Term:rdf_term, -String:string) is det.
%
% Ensures a String ― preferably a human readable one ― is returned for
% the given Term.

term_string(B, S, String) :-
  tp_chk(B, S, rdfs:label, Literal), !,
  term_string(B, Literal, String).
term_string(_, Name, String) :-
  rdf_name_string(Name, String).



%! tp(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object) is nondet.

tp(B1, S, P1, O1) :-
  pre_backend_(B1, B2),
  tp_predicate_dwim(P1, P2),
  tp_object_dwim(O1, O2),
  rdf_api:tp_(B2, S, P2, O2),
  post_backend_(B2, B1).

rdf_api:tp_(dummy, _, _, _) :-
  fail.



%! tp_chk(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object) is nondet.

tp_chk(B, S, P, O) :-
  once(tp(B, S, P, O)).



%! tp_count(?Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object, -N:nonneg) is nondet.

tp_count(B1, S, P1, O1, N) :-
  pre_backend_(B1, B2),
  tp_predicate_dwim(P1, P2),
  tp_object_dwim(O1, O2),
  rdf_api:tp_count_(B2, S, P2, O2, N),
  post_backend_(B2, B1).

rdf_api:tp_count_(dummy, _, _, _, 0) :-
  fail.



%! tp_dt(+Backend, ?S:rdf_subject, ?P:rdf_predicate, -Datetime:dt) is nondet.

tp_dt(B, S, P, Dt) :-
  tp(B, S, P, literal(type(D,Lex))),
  xsd_date_time_type(D),
  xsd_time_string(XsdDt, D, Lex),
  xsd_date_time(Dt, D, XsdDt).



%! tp_retractall(+Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object) is det.

tp_retractall(B1, S, P1, O1) :-
  tp_predicate_dwim(P1, P2),
  tp_object_dwim(O1, O2),
  pre_backend_(B1, B2),
  rdf_api:tp_retractall_(B2, S, P2, O2),
  post_backend_(B1, B2).

rdf_api:tp_retractall_(dummy, _, _, _) :-
  fail.



%! tp_string(?Backend, ?S:rdf_subject, ?P:rdf_predicate, -String:string) is nondet.

tp_string(B, S, P, String) :-
  tp(B, S, P, O),
  term_string(O, String).



%! tp_update(+Backend, ?S:rdf_subject, ?P:rdf_predicate, ?O:rdf_object, +Action:compound) is det.
%
% @arg Action is either of the following compound terms:
%
%   * datatype(iri)
%   * language_tag(atom)
%   * object(compound)
%   * predicate(rdf_predicate)
%   * subject(rdf_node)

tp_update(B, S, P, O1, datatype(D)) :- !,
  forall(
    tp(B, S, P, O1),
    (
      rdf_literal_lexical_form(O1, Lex),
      tp_retractall(B, S, P, O1),
      rdf_literal(D, _, Lex, O2),
      assert_triple(B, S, P, O2)
    )
  ).
tp_update(B, S, P, O1, ltag(LTag)) :- !,
  forall(
    tp(B, S, P, O1),
    (
      update_language_tagged_string_(O1, LTag, O2),
      tp_retractall(B, S, P, O1),
      assert_triple(B, S, P, O2)
    )
  ).
tp_update(B, S, P, O1, object(O2)) :- !,
  forall(
    tp(B, S, P, O1),
    (
      tp_retractall(B, S, P, O1),
      assert_triple(B, S, P, O2)
    )
  ).
tp_update(B, S, P1, O, predicate(P2)) :- !,
  forall(
    tp(B, S, P1, O),
    (
      tp_retractall(B, S, P1, O),
      assert_triple(B, S, P2, O)
    )
  ).
tp_update(B, S1, P, O, subject(S2)) :- !,
  forall(
    tp(B, S1, P, O),
    (
      tp_retractall(B, S1, P, O),
      assert_triple(B, S2, P, O)
    )
  ).

update_language_tagged_string_(literal(lang(_,Lex)), LTag, literal(lang(LTag,Lex))) :- !.
update_language_tagged_string_(literal(type(D,Lex)), LTag, literal(lang(LTag,Lex))) :-
  rdf_equal(D, xsd:string).



%! tp_value(?Backend, ?S:rdf_subject, ?P:rdf_predicate, -Value:term) is nondet.

tp_value(B, S, P, Value) :-
  tp(B, S, P, O),
  rdf_literal_value(O, Value).





% HELPERS %

%! pre_backend_(+Backend1, -Backend2) is det.

pre_backend_(Var, _) :-
  var(Var), !.
pre_backend_(B, B).



%! post_backend_(+Backend1, +Backend2) is det.

post_backend_(B1, _) :-
  ground(B1), !.
post_backend_(B, B).
