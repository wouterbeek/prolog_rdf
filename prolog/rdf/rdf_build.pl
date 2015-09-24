:- module(
  rdf_build,
  [
    bnode_literal/2, % ?Subject:bnode
                     % ?Literal:literal
    fresh_iri/2, % +Prefix, -Iri
    fresh_iri/3, % +Prefix:atom
                 % +SubPaths:list(atom)
                 % -Iri:iri
    rdf_assert_instance/2, % +Instance, ?Class
    rdf_assert_instance/3, % +Instance:rdf_term
                           % ?Class:or([iri,list(iri)])
                           % ?Graph:atom
    rdf_assert_literal/4, % +Subject, +Predicate, ?Datatype, +Value
    rdf_assert_literal/5, % +Subject:rdf_term
                          % +Predicate:iri
                          % ?Datatype:iri
                          % +Value
                          % ?Graph:atom
    rdf_assert_literal_pl/3, % +Subject, +Predicate, +Value
    rdf_assert_literal_pl/4, % +Subject:rdf_term
                            % +Predicate:iri
                            % +Value
                            % ?Graph:atom
    rdf_assert_now/2, % +Subject, +Predicate
    rdf_assert_now/3, % +Subject, +Predicate, ?Graph
    rdf_assert_now/4, % +Subject:iri
                      % +Predicate:iri
                      % +Datatype:iri
                      % ?Graph:atom
    rdf_assert_property/3, % +Property:iri
                           % ?Parent:iri
                           % ?Graph:atom
    rdf_assert2/4, % +Subject:rdf_term
                   % +Predicate:iri
                   % +Object:rdf_term
                   % ?Graph:atom
    rdf_retractall_literal/4, % ?Subject, ?Predicate:iri, ?Datatype, ?Value
    rdf_retractall_literal/5, % ?Subject:rdf_term
                              % ?Predicate:iri
                              % ?Datatype:iri
                              % ?Value
                              % ?Graph:atom
    rdf_retractall_resource/2, % +Resource:rdf_term
                               % ?Graph:atom
    rdf_retractall_term/1, % +Term
    rdf_retractall_term/2, % +Term:rdf_term
                           % ?Graph:atom
    rdf_retractall2/3, % ?Subject:rdf_term
                       % ?Predicate:iri
                       % ?Object:rdf_term
    rdf_retractall2/4 % ?Subject:rdf_term
                      % ?Predicate:iri
                      % ?Object:rdf_term
                      % ?Graph:atom
  ]
).
:- reexport(library(semweb/rdf_db)).

/** <module> RDF build

Simple asserion and retraction predicates for RDF.

@author Wouter Beek
@version 2015/07-2015/09
*/

:- use_module(library(owl/owl_read)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(uuid_ext)).
:- use_module(library(xsd/dateTime/xsd_dateTime_functions)).

%! bnode_literal(?Subject:bnode, ?Literal:compound) is nondet.

:- dynamic(bnode_literal/2).

:- rdf_meta(bnode_literal(?,o)).
:- rdf_meta(rdf_assert_instance(o,t)).
:- rdf_meta(rdf_assert_instance(o,t,?)).
:- rdf_meta(rdf_assert_literal(o,r,r,+)).
:- rdf_meta(rdf_assert_literal(o,r,r,+,?)).
:- rdf_meta(rdf_assert_literal_pl(o,r,+)).
:- rdf_meta(rdf_assert_literal_pl(o,r,+,?)).
:- rdf_meta(rdf_assert_now(o,r)).
:- rdf_meta(rdf_assert_now(o,r,+)).
:- rdf_meta(rdf_assert_now(o,r,r,+)).
:- rdf_meta(rdf_assert_property(o,r,?)).
:- rdf_meta(rdf_assert2(o,r,o,?)).
:- rdf_meta(rdf_retractall_literal(o,r,r,?)).
:- rdf_meta(rdf_retractall_literal(o,r,r,?,?)).
:- rdf_meta(rdf_retractall_resource(o,?)).
:- rdf_meta(rdf_retractall_term(o)).
:- rdf_meta(rdf_retractall_term(o,?)).
:- rdf_meta(rdf_retractall2(o,r,o)).
:- rdf_meta(rdf_retractall2(o,r,o,?)).





%! assert_bnode_literal(-Subject:bnode, +Literal:literal) is det.

assert_bnode_literal(S, Lit):-
  bnode_literal(S, Lit), !.
assert_bnode_literal(S, Lit):-
  rdf_bnode(S),
  assert(bnode_literal(S, Lit)).



%! fresh_iri(+Prefix:atom, -Iri:atom) is det.
% Succeeds with a fresh IRI within the RDF namespace denoted by Prefix.

fresh_iri(Prefix, Iri):-
  fresh_iri(Prefix, [], Iri).

%! fresh_iri(+Prefix:atom, +SubPaths:list(atom), -Iri:atom) is det.
% Succeeds with a fresh IRI within the RDF namespace denoted by Prefix
% and the given SubPaths.
%
% IRI freshness is guaranteed by the UUID that is used as the path suffix.
%
% @arg Prefix   A registered RDF prefix name.
% @arg SubPaths A list of path names that prefix the UUID.
% @arg Iri      A fresh IRI.

fresh_iri(Prefix, SubPaths0, Iri):-
  uuid_no_hyphen(Id),
  append(SubPaths0, [Id], SubPaths),
  atomic_list_concat(SubPaths, /, LocalName),

  % Resolve the absolute IRI against the base IRI denoted by the RDF prefix.
  rdf_global_id(Prefix:LocalName, Iri).



%! rdf_assert_instance(
%!   +Instance:rdf_term,
%!   ?Class:or([iri,list(iri)])
%! ) is det.

rdf_assert_instance(I, C):-
  rdf_assert_instance(I, C, _).

%! rdf_assert_instance(
%!   +Instance:rdf_term,
%!   ?Class:or([iri,list(iri)]),
%!   ?Graph:atom
%! ) is det.
% Asserts an instance/class relationship.
%
% The following triples are added to the database:
%
% ```nquads
% 〈TERM, rdf:type, CLASS, GRAPH〉
% ```

rdf_assert_instance(I, C, G):-
  (   var(C)
  ->  rdf_assert2(I, rdf:type, rdfs:'Resource', G)
  ;   is_list(C)
  ->  forall(member(C0, C), rdf_assert2(I, rdf:type, C0, G))
  ;   rdf_assert2(I, rdf:type, C, G)
  ).



%! rdf_assert_literal(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   ?Datatype:iri,
%!   +Value
%! ) is det.

rdf_assert_literal(S, P, D, V):-
  rdf_assert_literal(S, P, D, V, _).

%! rdf_assert_literal(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   ?Datatype:iri,
%!   +Value,
%!   ?Graph:atom
%! ) is det.
% Asserts a triple with a literal object term.
%
% Only emits canonical representations for XSD values.
%
% @compat RDF 1.1 Concepts and Abstract Syntax
% @compat XSD 1.1 Schema 2: Datatypes
% @ tbd Use 'Language-Tag'//1.

% Language-tagged strings.
rdf_assert_literal(S, P, rdf:langString, Lex-LTag, G):- !,
  rdf_assert2(S, P, literal(lang(LTag,Lex)), G).
% Simple literals (as per RDF 1.0 specification)
% assumed to be of type `xsd:string` (as per RDF 1.1 specification).
rdf_assert_literal(S, P, D, V, G):-
  var(D), !,
  rdf_assert_literal(S, P, xsd:string, V, G).
% Typed literals (as per RDF 1.0 specification).
rdf_assert_literal(S, P, D, Val, G):-
  rdf_canonical_map(D, Val, Lit),
  rdf_assert2(S, P, Lit, G).



%! rdf_assert_literal_pl(+Subject:rdf_term, +Predicate:iri, +Value) is det.

rdf_assert_literal_pl(S, P, V):-
  rdf_assert_literal_pl(S, P, V, _).

%! rdf_assert_literal_pl(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Value,
%!   ?Graph:atom
%! ) is det.
% Guess an appropriate RDF datatype for serializing Value.
% Since RDF has a more granual datatype system than Prolog
% this is only a fair guess.
%
% This works for the following Prolog types:
%   * atom
%   * date-time compound terms date/3, date/9 and time/3
%   * float
%   * HTML DOM
%   * integer
%   * Pair of two atoms where the former denotes a language tag
%     and the latter denotes a text string.
%   * rational
%   * string
%   * XML DOM

% Specifically added support for SWI7 string.
% Notice that library plXsd uses Prolog atoms
% as the values of xsd:string literals.
rdf_assert_literal_pl(S, P, V0, G):-
  string(V0), !,
  atom_string(V, V0),
  rdf_assert_literal_pl(S, P, V, G).
rdf_assert_literal_pl(S, P, V, G):-
  rdf_guess_datatype(V, D), !,
  rdf_assert_literal(S, P, D, V, G).
rdf_assert_literal_pl(_, _, V, _):-
  print_message(error, cannot_guess_rdf_datatype(V)).



%! rdf_assert_now(+Subject:rdf_term, +Predicate:iri) is det.

rdf_assert_now(S, P):-
  rdf_assert_now(S, P, _).


%! rdf_assert_now(+Subject:rdf_term, +Predicate:iri, ?Graph:atom) is det.

rdf_assert_now(S, P, G):-
  rdf_assert_now(S, P, xsd:dateTime, G).


%! rdf_assert_now(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Datatype:iri,
%!   ?Graph:atom
%! ) is det.

rdf_assert_now(S, P, D, G):-
  get_time(NowFloat),
  stamp_date_time(NowFloat, Now, local),
  rdf_assert_literal(S, P, D, Now, G).


%! rdf_assert_property(+Property:iri, ?Parent:iri, ?Graph:atom) is det.
% Asserts an RDF property.
%
% The following triples are added to the database:
%
% ```nquads
% 〈TERM, rdf:type, rdf:Property, GRAPH〉
% ```

rdf_assert_property(P, Parent, G):-
  rdf_defval(rdf:'Property', Parent),
  rdf_assert_instance(P, Parent, G).



%! rdf_assert2(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.
% Alternative of rdf/4 that allows Graph to be uninstantiated.
%
% @see rdf_db:rdf/4

rdf_assert2(S0, P, O, G):-
  rdf_is_literal(S0), !,
  assert_bnode_literal(S, S0),
  rdf_assert2(S, P, O, G).
rdf_assert2(S, P, O, G):-
  var(G), !,
  rdf_assert(S, P, O).
rdf_assert2(S, P, O, G):-
  rdf_assert(S, P, O, G).



%! rdf_retractall_literal(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value
%! ) is det.

rdf_retractall_literal(S, P, D, V):-
  rdf_retractall_literal(S, P, D, V, _).


%! rdf_retractall_literal(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value,
%!   ?Graph:atom
%! ) is det.
% Retracts all matching RDF triples that have literal object terms.
%
% Implementation note: this assumes that simple literals are always
% asserted with datatype IRI `xsd:string`.
% We do not retract literal compound terms of the form
% `literal(LexicalForm:atom)`.

% If no RDF datatype is given we assume XSD string,
% as specified by the RDF 1.1 standard.
rdf_retractall_literal(S, P, D, V, G):-
  forall(
    rdf_read:rdf_literal(S, P, D, V, G, T),
    rdf_retractall_term(T, G)
  ).



%! rdf_retractall_resource(+Resource:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the resource denoted by the given RDF term
%  occurs.

rdf_retractall_resource(T, G):-
  forall(
    owl_id(T, T0),
    rdf_retractall_term(T0, G)
  ).



%! rdf_retractall_term(+Term:rdf_term) is det.
% Wrapper around rdf_retractall_term/2 with uninstantiated graph.

rdf_retractall_term(T):-
  rdf_retractall_term(T, _).

%! rdf_retractall_term(+Term:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the given RDF term occurs.

rdf_retractall_term(T, G):-
  rdf_retractall2(T, _, _, G),
  rdf_retractall2(_, T, _, G),
  rdf_retractall2(_, _, T, G).



%! rdf_retractall2(+Subject:rdf_term, +Predicate:iri, +Object:rdf_term) is det.

rdf_retractall2(S0, P, O):-
  rdf_is_literal(S0), !,
  bnode_literal(S, S0),
  rdf_retractall2(S, P, O).
rdf_retractall2(S, P, O):-
  rdf_retractall(S, P, O).


%! rdf_retractall2(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.

rdf_retractall2(S0, P, O, G):-
  rdf_is_literal(S0), !,
  bnode_literal(S, S0),
  rdf_retractall2(S, P, O, G).
rdf_retractall2(S, P, O, G):-
  rdf_retractall(S, P, O, G).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(cannot_guess_rdf_datatype(V)) -->
  ['Cannot determine RDF datatype for Prolog value ~q~n.'-[V]].
