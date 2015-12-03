:- module(
  rdf_build,
  [
    fresh_iri/2, % +Prefix, -Iri
    fresh_iri/3, % +Prefix:atom
                 % +SubPaths:list(atom)
                 % -Iri:iri
    grdf_assert/3, % +Subject, +Predicate, +Object
    grdf_assert/4, % +Subject:gid
                   % +Predicate:gid
                   % +Object:gid
                   % ?Graph:rdf_graph
    grdf_retractall/1, % +Statement:rdf_stmt
    grdf_retractall/3, % ?Subject, ?Predicate, ?Object
    grdf_retractall/4, % ?Subject:gid
                       % ?Predicate:gid
                       % ?Object:gid
                       % ?Graph:rdf_graph
    rdf_assert_instance/2, % +Instance, ?Class
    rdf_assert_instance/3, % +Instance:rdf_term
                           % ?Classes:or([rdf_term,list(rdf_term)])
                           % ?Graph:rdf_graph
    rdf_assert_literal/4, % +Subject, +Predicate, ?Datatype, +Value
    rdf_assert_literal/5, % +Subject:rdf_term
                          % +Predicate:iri
                          % ?Datatype:iri
                          % +Value
                          % ?Graph:rdf_graph
    rdf_assert_literal_pl/3, % +Subject, +Predicate, +Value
    rdf_assert_literal_pl/4, % +Subject:rdf_term
                            % +Predicate:iri
                            % +Value
                            % ?Graph:rdf_graph
    rdf_assert_now/2, % +Subject, +Predicate
    rdf_assert_now/3, % +Subject, +Predicate, ?Graph
    rdf_assert_now/4, % +Subject:rdf_term
                      % +Predicate:iri
                      % +Datatype:iri
                      % ?Graph:rdf_graph
    rdf_assert_property/3, % +Property:iri
                           % ?Parent:iri
                           % ?Graph:rdf_graph
    rdf_retractall_literal/4, % ?Subject, ?Predicate:iri, ?Datatype, ?Value
    rdf_retractall_literal/5, % ?Subject:rdf_term
                              % ?Predicate:iri
                              % ?Datatype:iri
                              % ?Value
                              % ?Graph:rdf_graph
    rdf_retractall_term/1, % +Term
    rdf_retractall_term/2 % +Term:rdf_term
                          % ?Graph:rdf_graph
  ]
).
:- reexport(library(semweb/rdf_db), except([rdf/3,rdf/4])).

/** <module> Generalized RDF building

Simple asserion and retraction predicates for RDF.

@author Wouter Beek
@license MIT License
@version 2015/07-2015/10, 2015/12
*/

:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(typecheck)).
:- use_module(library(uuid_ext)).
:- use_module(library(xsd/dateTime/xsd_dateTime_functions)).

:- rdf_meta(grdf_assert(o,+,o)).
:- rdf_meta(grdf_assert(o,+,o,r)).
:- rdf_meta(grdf_retractall(t)).
:- rdf_meta(grdf_retractall(o,+,o)).
:- rdf_meta(grdf_retractall(o,+,o,r)).
:- rdf_meta(rdf_assert_instance(o,t)).
:- rdf_meta(rdf_assert_instance(o,t,r)).
:- rdf_meta(rdf_assert_literal(o,r,r,+)).
:- rdf_meta(rdf_assert_literal(o,r,r,+,r)).
:- rdf_meta(rdf_assert_literal_pl(o,r,+)).
:- rdf_meta(rdf_assert_literal_pl(o,r,+,r)).
:- rdf_meta(rdf_assert_now(o,r)).
:- rdf_meta(rdf_assert_now(o,r,r)).
:- rdf_meta(rdf_assert_now(o,r,r,r)).
:- rdf_meta(rdf_assert_property(o,r,r)).
:- rdf_meta(rdf_retractall_literal(o,r,r,?)).
:- rdf_meta(rdf_retractall_literal(o,r,r,?,r)).
:- rdf_meta(rdf_retractall_term(o)).
:- rdf_meta(rdf_retractall_term(o,r)).





%! fresh_iri(+Prefix:atom, -Iri:atom) is det.
% Wrapper around fresh_iri/3 using no subpaths.

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



%! grdf_assert(+Statement:rdf_stmt) is det.
% Wrapper around grdf_assert/[3,4].
% Statement is of the form `rdf/[3,4]`.

grdf_assert(rdf(S,P,O)):- !,
  grdf_assert(S, P, O).
grdf_assert(rdf(S,P,O,G)):-
  grdf_assert(S, P, O, G).


%! grdf_assert(+Subject:rdf_term, +Predicate:iri, +Object:rdf_term) is det.
% Wrapper around grdf_assert/4 with uninstantiated graph.

grdf_assert(S, P, O):-
  grdf_assert(S, P, O, _).


%! grdf_assert(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:rdf_graph
%! ) is det.
% Alternative of Semweb's rdf_assert/4 that allows literals to appear
% in the subject positions.

grdf_assert(S, P, O, G):-
  maplist(rdf_normalize, [S,P,O], [SNorm,PNorm,ONorm]),
  maplist(id_assign_term, [SNorm,PNorm,ONorm], [Sid,Pid,Oid]),
  grdf_assert_id(Sid, Pid, Oid, G).

% 1. Identity statements.
grdf_assert_id(Sid1, Pid, Oid, _):-
  term_to_id(owl:sameAs, Pid), !,
  (rdf_is_literal(Sid1) -> id_assign_literal(Sid1, Sid2), Lit = true ; true),
  (rdf_is_literal(Oid) -> Lit = true ; true),
  % Literals are not in the identity store.  They are stored directly.
  (Lit == true -> rdf_assert(Sid2, Pid, Oid) ; id_store(Sid1, Oid)).
% 2. Statements other than the identity statement:
% 2a. In generalized RDF a literal may appear in the subject position.
grdf_assert_id(Lit, Pid, Oid, G):-
  rdf_is_literal(Lit), !,
  id_assign_literal(Lit, Sid),
  rdf_assert(Sid, Pid, Oid, G).
% 2b. Non-generalized triple.
grdf_assert_id(Sid, Pid, Oid, G):-
  var(G), !,
  rdf_assert(Sid, Pid, Oid).
% 2c. Non-generalized quadruple.
grdf_assert_id(Sid, Pid, Oid, G):-
  rdf_assert(Sid, Pid, Oid, G).



%! grdf_retractall(+Statement:rdf_stmt) is det.

grdf_retractall(rdf(S,P,O)):- !,
  grdf_retractall(S, P, O).
grdf_retractall(rdf(S,P,O,G)):-
  grdf_retractall(S, P, O, G).


%! grdf_retractall(?Subject:rdf_term, ?Predicate:iri, ?Object:rdf_term) is det.

grdf_retractall(S, P, O):-
  grdf_retractall(S, P, O, _).


%! grdf_retractall(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is det.

grdf_retractall(S, P, O, G):-
  (rdf_is_literal(S) -> id_to_literal(Sid, S) ; var_or_term_to_id(S, Sid)),
  maplist(var_or_term_to_id, [P,O], [Pid,Oid]),
  rdf_retractall(Sid, Pid, Oid, G),
  maplist(id_remove, [Sid,Pid,Oid]).



%! rdf_assert_instance(
%!   +Instance:rdf_term,
%!   ?Classes:or([rdf_term,list(rdf_term)])
%! ) is det.
% Wrapper around rdf_assert_instance/3 with uninstantiated graph.

rdf_assert_instance(I, C):-
  rdf_assert_instance(I, C, _).


%! rdf_assert_instance(
%!   +Instance:rdf_term,
%!   ?Classes:or([rdf_term,list(rdf_term)]),
%!   ?Graph:rdf_graph
%! ) is det.
% Asserts an instance/class relationship.
%
% The following triples are added to the database:
%
% ```nquads
% 〈TERM, rdf:type, CLASS, GRAPH〉
% ```

rdf_assert_instance(I, C, G):-
  var(C), !,
  grdf_assert(I, rdf:type, rdfs:'Resource', G).
rdf_assert_instance(I, Cs, G):-
  is_list(Cs), !,
  forall(member(C, Cs), grdf_assert(I, rdf:type, C, G)).
rdf_assert_instance(I, C, G):-
  rdf_assert_instance(I, [C], G).



%! rdf_assert_literal(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   ?Datatype:iri,
%!   +Value
%! ) is det.
% Wrapper around rdf_assert_literal/5 with uninstantiated graph.

rdf_assert_literal(S, P, D, V):-
  rdf_assert_literal(S, P, D, V, _).


%! rdf_assert_literal(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   ?Datatype:iri,
%!   +Value,
%!   ?Graph:rdf_graph
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
  grdf_assert(S, P, literal(lang(LTag,Lex)), G).
% Simple literals (as per RDF 1.0 specification)
% assumed to be of type `xsd:string` (as per RDF 1.1 specification).
rdf_assert_literal(S, P, D, V, G):-
  var(D), !,
  rdf_assert_literal(S, P, xsd:string, V, G).
% Typed literals (as per RDF 1.0 specification).
rdf_assert_literal(S, P, D, Val, G):-
  rdf_canonical_map(D, Val, Lit),
  grdf_assert(S, P, Lit, G).



%! rdf_assert_literal_pl(+Subject:rdf_term, +Predicate:iri, +Value) is det.
% Wrapper around rdf_assert_literal_pl/4 with uninstantiated graph.

rdf_assert_literal_pl(S, P, V):-
  rdf_assert_literal_pl(S, P, V, _).


%! rdf_assert_literal_pl(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Value,
%!   ?Graph:rdf_graph
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
% Wrapper around rdf_assert_now/3 with uninstantiated graph.

rdf_assert_now(S, P):-
  rdf_assert_now(S, P, _).


%! rdf_assert_now(+Subject:rdf_term, +Predicate:iri, ?Graph:rdf_graph) is det.
% Wrapper around rdf_assert_now/4 with datatype `xsd:dateTime`.

rdf_assert_now(S, P, G):-
  rdf_assert_now(S, P, xsd:dateTime, G).


%! rdf_assert_now(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Datatype:iri,
%!   ?Graph:rdf_graph
%! ) is det.
% Asserts the triple `〈Subject, Predicate, 〈Datatype,Lex〉〉@Graph`
% where `Lex` denotes the date-time point of assertion
% under the given Datatype.

rdf_assert_now(S, P, D, G):-
  get_time(NowFloat),
  stamp_date_time(NowFloat, Now, local),
  rdf_assert_literal(S, P, D, Now, G).



%! rdf_assert_property(+Property:iri, ?Parent:iri, ?Graph:rdf_graph) is det.
% Asserts an RDF Property that belongs to a class of properties called Parent.
%
% Parent defaults to `rdf:Property`.

rdf_assert_property(P, Parent, G):-
  rdf_defval(rdf:'Property', Parent),
  rdf_assert_instance(P, Parent, G).



%! rdf_retractall_literal(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value
%! ) is det.
% Wrapper around rdf_retractall_literal/5 with uninstantiated graph.

rdf_retractall_literal(S, P, D, V):-
  rdf_retractall_literal(S, P, D, V, _).


%! rdf_retractall_literal(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value,
%!   ?Graph:rdf_graph
%! ) is det.
% Retracts all matching RDF triples that have a literal object term
% with the given Datatype and/or Value.
%
% This assumes that simple literals are always
% asserted with datatype IRI `xsd:string`.
% We do not retract literal compound terms of the form
% `literal(LexicalForm:atom)`.
%
% If no RDF datatype is given we assume XSD string,
% as specified by the RDF 1.1 standard.

rdf_retractall_literal(S, P, D, V, G):-
  forall(
    % Use a private predicate that returns the matched quadruple
    % as a compound term.
    rdf_read:rdf_literal(S, P, D, V, G, Quad),
    grdf_retractall(Quad)
  ).



%! rdf_retractall_term(+Term:rdf_term) is det.
% Wrapper around rdf_retractall_term/2 with uninstantiated graph.

rdf_retractall_term(T):-
  rdf_retractall_term(T, _).


%! rdf_retractall_term(+Term:rdf_term, ?Graph:rdf_graph) is det.
% Removes all triples in which the given RDF term occurs.

rdf_retractall_term(T, G):-
  grdf_retractall(T, _, _, G),
  grdf_retractall(_, T, _, G),
  grdf_retractall(_, _, T, G).





% HELPERS %

%! rdf_normalize(+Term:rdf_term, -NormalizedTerm:rdf_term) is det.

rdf_normalize(X, Y):-
  is_iri(X), !,
  iri_normalized(X, Y).
rdf_normalize(X, X).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(cannot_guess_rdf_datatype(V)) -->
  ['Cannot determine RDF datatype for Prolog value ~q~n.'-[V]].
