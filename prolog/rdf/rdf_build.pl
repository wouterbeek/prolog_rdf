:- module(
  rdf_build,
  [
    rdf_assert/1, % +Statement:rdf_stmt
    rdf_assert/3, % +Subject, +Predicate, +Object
    rdf_assert/4, % +Subject:gid
                  % +Predicate:gid
                  % +Object:gid
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
    rdf_create_iri/2, % +Prefix, -Iri
    rdf_create_iri/3, % +Prefix:atom
                      % +SubPaths:list(atom)
                      % -Iri:atom
    rdf_retractall/1, % +Statement:rdf_stmt
    rdf_retractall/3, % ?Subject, ?Predicate, ?Object
    rdf_retractall/4, % ?Subject:gid
                      % ?Predicate:gid
                      % ?Object:gid
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
:- reexport(library(semweb/rdf_db), [
     rdf_bnode/1 as rdf_create_bnode % -BlankNode:rdf_bnode
   ]).

/** <module> Generalized RDF building

Simple asserion and retraction predicates for RDF.

@author Wouter Beek
@compat RDF 1.1
@license MIT License
@version 2015/07-2015/10, 2015/12
*/

:- use_module(library(datetime/datetime)).
:- use_module(library(default)).
:- use_module(library(rdf/id_store)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(semweb/rdf_db), [
     rdf_assert/3 as rdf_assert_id, % +Subject, +Predicate, +Object
     rdf_assert/4 as rdf_assert_id, % +Subject:or([rdf_bnode,iri])
                                    % +Predicate:iri
                                    % +Object:rdf_term
                                    % +Graph:atom
     rdf_retractall/4 as rdf_retractall_id % ?Subject:or([rdf_bnode,iri])
                                           % ?Predicate:iri
                                           % ?Object:rdf_term
                                           % ?Graph:atom
   ]).
:- use_module(library(typecheck)).
:- use_module(library(uuid_ext)).
:- use_module(library(xsd/datetime/xsd_datetime_functions)).

:- rdf_meta(rdf_assert(t)).
:- rdf_meta(rdf_assert(o,r,o)).
:- rdf_meta(rdf_assert(o,r,o,r)).
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
:- rdf_meta(rdf_retractall(t)).
:- rdf_meta(rdf_retractall(o,+,o)).
:- rdf_meta(rdf_retractall(o,+,o,r)).
:- rdf_meta(rdf_retractall_literal(o,r,r,?)).
:- rdf_meta(rdf_retractall_literal(o,r,r,?,r)).
:- rdf_meta(rdf_retractall_term(o)).
:- rdf_meta(rdf_retractall_term(o,r)).






%! rdf_assert(+Statement:rdf_stmt) is det.
% Wrapper around rdf_assert/[3,4].
% Statement is of the form `rdf/[3,4]`.

rdf_assert(rdf(S,P,O)):- !,
  rdf_assert(S, P, O).
rdf_assert(rdf(S,P,O,G)):-
  rdf_assert(S, P, O, G).


%! rdf_assert(+Subject:rdf_term, +Predicate:iri, +Object:rdf_term) is det.
% Wrapper around rdf_assert/4 that asserts in the default graph.

rdf_assert(S, P, O):-
  rdf_assert(S, P, O, default).


%! rdf_assert(
%!   +Subject:rdf_term,
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:rdf_graph
%! ) is det.
% Alternative of Semweb's rdf_assert/4 that allows literals to appear
% in the subject positions.

% 1. Identity statements.
rdf_assert(S, P, O, _):-
  rdf_is_id(owl:sameAs, P), !,
  store_id(S, O).
% 2. Statements other than the identity statement.
rdf_assert(S, P, O, G):-
  (rdf_is_literal(S) -> assign_literal_id(S, Sid) ; assign_term_id(S, Sid)),
  maplist(assign_term_id, [P,O], [Pid,Oid]),
  defval(default, G),
  rdf_assert_id(Sid, Pid, Oid, G).
  


%! rdf_create_iri(+Prefix:atom, -Iri:atom) is det.
% Wrapper around rdf_create_iri/3 using no subpaths.

rdf_create_iri(Prefix, Iri):-
  rdf_create_iri(Prefix, [], Iri).


%! rdf_create_iri(+Prefix:atom, +SubPaths:list(atom), -Iri:atom) is det.
% Succeeds with a fresh IRI within the RDF namespace denoted by Prefix
% and the given SubPaths.
%
% IRI freshness is guaranteed by the UUID that is used as the path suffix.
%
% @arg Prefix   A registered RDF prefix name.
% @arg SubPaths A list of path names that prefix the UUID.
% @arg Iri      A fresh IRI.

rdf_create_iri(Prefix, SubPaths0, Iri):-
  uuid_no_hyphen(Id),
  append(SubPaths0, [Id], SubPaths),
  atomic_list_concat(SubPaths, /, LocalName),
  % Resolve the absolute IRI against the base IRI denoted by the RDF prefix.
  rdf_expand_rt(Prefix:LocalName, Iri).



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
  rdf_assert(I, rdf:type, rdfs:'Resource', G).
rdf_assert_instance(I, Cs, G):-
  is_list(Cs), !,
  forall(member(C, Cs), rdf_assert(I, rdf:type, C, G)).
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
  rdf_assert(S, P, literal(lang(LTag,Lex)), G).
% Simple literals (as per RDF 1.0 specification)
% assumed to be of type `xsd:string` (as per RDF 1.1 specification).
rdf_assert_literal(S, P, D, V, G):-
  var(D), !,
  rdf_assert_literal(S, P, xsd:string, V, G).
% Typed literals (as per RDF 1.0 specification).
rdf_assert_literal(S, P, D, Val, G):-
  rdf_canonical_map(D, Val, Lit),
  rdf_assert(S, P, Lit, G).



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
  get_datetime(Now),
  rdf_assert_literal(S, P, D, Now, G).



%! rdf_assert_property(+Property:iri, ?Parent:iri, ?Graph:rdf_graph) is det.
% Asserts an RDF Property that belongs to a class of properties called Parent.
%
% Parent defaults to `rdf:Property`.

rdf_assert_property(P, Parent, G):-
  rdf_defval(rdf:'Property', Parent),
  rdf_assert_instance(P, Parent, G).



%! rdf_retractall(+Statement:rdf_stmt) is det.

rdf_retractall(rdf(S,P,O)):- !,
  rdf_retractall(S, P, O).
rdf_retractall(rdf(S,P,O,G)):-
  rdf_retractall(S, P, O, G).


%! rdf_retractall(?Subject:rdf_term, ?Predicate:iri, ?Object:rdf_term) is det.

rdf_retractall(S, P, O):-
  rdf_retractall(S, P, O, _).


%! rdf_retractall(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Object:rdf_term,
%!   ?Graph:atom
%! ) is det.

rdf_retractall(S, P, O, G):-
  (rdf_is_literal(S) -> literal_id(S, Sid) ; term_to_id0(S, Sid)),
  maplist(term_to_id0, [P,O], [Pid,Oid]),
  rdf_retractall_id(Sid, Pid, Oid, G),
  maplist(remove_id, [Sid,Pid,Oid]).
term_to_id0(T, _):- var(T), !.
term_to_id0(T, TId):- term_to_id(T, TId).



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
    rdf_retractall(Quad)
  ).



%! rdf_retractall_term(+Term:rdf_term) is det.
% Wrapper around rdf_retractall_term/2 with uninstantiated graph.

rdf_retractall_term(T):-
  rdf_retractall_term(T, _).


%! rdf_retractall_term(+Term:rdf_term, ?Graph:rdf_graph) is det.
% Removes all triples in which the given RDF term occurs.

rdf_retractall_term(T, G):-
  rdf_retractall(T, _, _, G),
  rdf_retractall(_, T, _, G),
  rdf_retractall(_, _, T, G).





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
