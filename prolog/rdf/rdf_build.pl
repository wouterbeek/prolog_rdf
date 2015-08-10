:- module(
  rdf_build,
  [
    fresh_iri/2, % +Prefix, -Iri
    fresh_iri/3, % +Prefix:atom
                 % +SubPaths:list(atom)
                 % -Iri:iri
    rdf_assert_instance/3, % +Instance:or([bnode,iri])
                           % ?Class:iri
                           % ?Graph:atom
    rdf_assert_literal0/3, % +Subject, +Predicate, +Value
    rdf_assert_literal0/4, % +Subject:or([bnode,iri])
                           % +Predicate:iri
                           % +Value
                           % ?Graph:atom
    rdf_assert_literal/4, % +Subject, +Predicate, ?Datatype, +Value
    rdf_assert_literal/5, % +Subject:or([bnode,iri])
                          % +Predicate:iri
                          % ?Datatype:iri
                          % +Value
                          % ?Graph:atom
    rdf_assert_now/3, % +Subject, +Predicate, +Graph
    rdf_assert_now/4, % +Subject:iri
                      % +Predicate:iri
                      % +Datatype:iri
                      % +Graph:atom
    rdf_assert_property/3, % +Property:iri
                           % ?Parent:iri
                           % ?Graph:atom
    rdf_assert2/4, % +Subject:or([bnode,iri])
                   % +Predicate:iri
                   % +Object:rdf_term
                   % ?Graph:atom
    rdf_retractall_literal/4, % ?Subject, ?Predicate:iri, ?Datatype, ?Value
    rdf_retractall_literal/5, % ?Subject:or([bnode,iri])
                              % ?Predicate:iri
                              % ?Datatype:iri
                              % ?Value
                              % ?Graph:atom
    rdf_retractall_resource/2, % +Resource:rdf_term
                               % ?Graph:atom
    rdf_retractall_term/2 % +Term:rdf_term
                          % ?Graph:atom
  ]
).
:- reexport(library(semweb/rdf_db)).

/** <module> RDF build

Simple asserion and retraction predicates for RDF.

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(default)).
:- use_module(library(owl/owl_read)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(uri)).
:- use_module(library(uuid_ext)).
:- use_module(library(xsd/dateTime/xsd_dateTime_functions)).

:- rdf_meta(rdf_assert_instance(r,r,?)).
:- rdf_meta(rdf_assert_literal0(r,r,+)).
:- rdf_meta(rdf_assert_literal0(r,r,+,?)).
:- rdf_meta(rdf_assert_literal(r,r,r,+)).
:- rdf_meta(rdf_assert_literal(r,r,r,+,?)).
:- rdf_meta(rdf_assert_now(r,r,+)).
:- rdf_meta(rdf_assert_now(r,r,r,+)).
:- rdf_meta(rdf_assert_property(r,r,?)).
:- rdf_meta(rdf_assert2(r,r,o,?)).
:- rdf_meta(rdf_retractall_literal(r,r,r,?)).
:- rdf_meta(rdf_retractall_literal(r,r,r,?,?)).
:- rdf_meta(rdf_retractall_resource(r,?)).
:- rdf_meta(rdf_retractall_term(r,?)).





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
%!   +Instance:or([bnode,iri]),
%!   ?Class:iri,
%!   ?Graph:atom
%! ) is det.
% Asserts an instance/class relationship.
%
% The following triples are added to the database:
%
% ```nquads
% <TERM,rdf:type,CLASS,GRAPH>
% ```
%
% @arg Instance A resource-denoting subject term (IRI or blank node).
% @arg Class    A class-denoting IRI or `rdfs:Resource` if uninstantiated.
% @arg Graph    The atomic name of an RDF graph or `user` if uninstantiated.

rdf_assert_instance(I, C, G):-
  rdf_defval(rdfs:'Resource', C),
  rdf_assert2(I, rdf:type, C, G).



%! rdf_assert_literal0(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value
%! ) is det.

rdf_assert_literal0(S, P, V):-
  rdf_assert_literal0(S, P, V, _).

%! rdf_assert_literal0(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Value,
%!   ?Graph:atom
%! ) is det.
% Guess an appropriate RDF datatype for serializing Value.
% Since RDF has a more granual datatype system than Prolog
% this is only a fail guess.

rdf_assert_literal0(S, P, V, G):-
  rdf_guess_datatype(V, D),
  rdf_assert_literal(S, P, D, V, G).



%! rdf_assert_literal(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   ?Datatype:iri,
%!   +Value
%! ) is det.

rdf_assert_literal(S, P, D, V):-
  rdf_assert_literal(S, P, D, V, _).

%! rdf_assert_literal(
%!   +Subject:or([bnode,iri]),
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

% Language-tagged strings.
rdf_assert_literal(S, P, rdf:langString, LangTag0-Lex, G):-
  % @ tbd Use 'Language-Tag'//1.
  atomic_list_concat(LangTag0, -, LangTag),
  rdf_assert2(S, P, literal(lang(LangTag,Lex)), G).
% Simple literals (as per RDF 1.0 specification)
% assumed to be of type `xsd:string` (as per RDF 1.1 specification).
rdf_assert_literal(S, P, D, V, G):-
  var(D), !,
  rdf_assert_literal(S, P, xsd:string, V, G).
% Typed literals (as per RDF 1.0 specification).
rdf_assert_literal(S, P, D, V, G):-
  rdf_canonical_map(D, V, Lex),
  rdf_assert2(S, P, literal(type(D,Lex)), G).



%! rdf_assert_now(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Graph:atom
%! ) is det.

rdf_assert_now(S, P, G):-
  rdf_assert_now(S, P, xsd:dateTime, G).


%! rdf_assert_now(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Datatype:iri,
%!   +Graph:atom
%! ) is det.

rdf_assert_now(S, P, D, G):-
  get_time(NowFloat),
  stamp_date_time(NowFloat, Now0, 0),
  prolog_xsd_date(Now0, Now),
  rdf_assert_literal(S, P, D, Now, G).


%! rdf_assert_property(+Property:iri, ?Parent:iri, ?Graph:atom) is det.
% Asserts an RDF property.
%
% The following triples are added to the database:
%
% ```nquads
% <TERM,rdf:type,rdf:Property,GRAPH>
% ```

rdf_assert_property(P, Parent, G):-
  rdf_defval(rdf:'Property', Parent),
  rdf_assert_instance(P, Parent, G).



%! rdf_assert2(
%!   +Subject:or([bnode,iri]),
%!   +Predicate:iri,
%!   +Object:rdf_term,
%!   ?Graph:atom
%! ) is det.
% Alternative of rdf/4 that allows Graph to be uninstantiated.
%
% @see rdf_db:rdf/4

rdf_assert2(S, P, O, G):-
  var(G), !,
  rdf_assert(S, P, O).
rdf_assert2(S, P, O, G):-
  rdf_assert(S, P, O, G).



%! rdf_retractall(+Triple:compound) is det.

rdf_retractall(rdf(S,P,O)):- !,
  rdf_retractall(S, P, O).
rdf_retractall(rdf(S,P,O,G)):- !,
  rdf_retractall(S, P, O, G).



%! rdf_retractall_literal(
%!   ?Subject:or([bnode,iri]),
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value
%! ) is det.

rdf_retractall_literal(S, P, D, V):-
  rdf_retractall_literal(S, P, D, V, _).


%! rdf_retractall_literal(
%!   ?Subject:or([bnode,iri]),
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
    rdf_literal(S, P, D, V, G, T),
    rdf_retractall(T)
  ).



%! rdf_retractall_resource(+Resource:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the resource denoted by the given RDF term
%  occurs.

rdf_retractall_resource(T, G):-
  forall(
    owl_id(T, T0),
    rdf_retractall_term(T0, G)
  ).



%! rdf_retractall_term(+Term:rdf_term, ?Graph:atom) is det.
% Removes all triples in which the given RDF term occurs.

rdf_retractall_term(Term, G):-
  rdf_retractall(Term, _, _, G),
  rdf_retractall(_, Term, _, G),
  rdf_retractall(_, _, Term, G).
