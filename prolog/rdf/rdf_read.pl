:- module(
  rdf_read,
  [
    rdf_date/3, % ?Subject, ?Predicate, ?Date
    rdf_date/4, % ?Subject:rdf_term
                % ?Predicate:iri
                % ?Date:compound
                % ?Graph:atom
    rdf_instance/2, % ?Instance, ?Class
    rdf_instance/3, % ?Instance:iri
                    % ?Class:iri
                    % ?Graph:atom
    rdf_langstring/4, % ?Subject, ?Predicate, +LanguagePriorityList, ?Value
    rdf_langstring/5, % ?Subject:rdf_term
                      % ?Predicate:iri
                      % +LanguagePriorityList:list(atom)
                      % ?Value:pair(atom)
                      % ?Graph:atom
    rdf_literal/3, % ?Subject, ?Predicate, ?Value
    rdf_literal/4, % ?Subject, ?Predicate, ?Datatype, ?Value
    rdf_literal/5, % ?Subject:rdf_term
                   % ?Predicate:iri
                   % ?Datatype:iri
                   % ?Value
                   % ?Graph:atom
    rdf_literal_pl/3, % ?Subject, ?Predicate, ?Value
    rdf_literal_pl/4, % ?Subject, ?Predicate, ?Datatype, ?Value
    rdf_literal_pl/5 % ?Subject:rdf_term
                     % ?Predicate:iri
                     % ?Datatype:iri
                     % ?Value
                     % ?Graph:atom
  ]
).

/** <module> RDF read

@author Wouter Beek
@compat [RDF 1.1 Concepts and Abstract Syntax](http://www.w3.org/TR/rdf11-concepts/)
@license MIT License
@version 2015/07-2015/10
*/

:- use_module(library(error)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(xsd/dateTime/xsd_dateTime_functions)).
:- use_module(library(xsd/xsd)).

:- rdf_meta(rdf_date(o,r,?)).
:- rdf_meta(rdf_date(o,r,?,?)).
:- rdf_meta(rdf_instance(o,r)).
:- rdf_meta(rdf_instance(o,r,?)).
:- rdf_meta(rdf_langstring(o,r,+,?)).
:- rdf_meta(rdf_langstring(o,r,+,?,?)).
:- rdf_meta(rdf_literal(o,r,r)).
:- rdf_meta(rdf_literal(o,r,r,?)).
:- rdf_meta(rdf_literal(o,r,r,?,?)).
:- rdf_meta(rdf_literal_pl(o,r,r)).
:- rdf_meta(rdf_literal_pl(o,r,r,?)).
:- rdf_meta(rdf_literal_pl(o,r,r,?,?)).

:- multifile(error:has_type/2).
error:has_type(rdf_term, Term):-
  (   rdf_is_bnode(Term)
  ;   rdf_is_literal(Term)
  ;   rdf_is_resource(Term)
  ).





%! rdf_date(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Date:compound
%! ) is nondet.
% Wrapper around rdf_date/4.

rdf_date(S, P, V):-
  rdf_date(S, P, V, _).


%! rdf_date(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Date:compound,
%!   ?Graph:atom
%! ) is nondet.
% Read some date-time value.
%
% Supports the following RDF datatypes:
%   * xsd:date
%   * xsd:dateTime
%   * xsd:gDay
%   * xsd:gMonth
%   * xsd:gMonthDay
%   * xsd:gYear
%   * xsd:gYearMonth
%   * xsd:time

rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:date, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:dateTime, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gDay, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gMonth, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gMonthDay, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gYear, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:gYearMonth, V, G).
rdf_date(S, P, V, G):-
  rdf_literal(S, P, xsd:time, V, G).



%! rdf_instance(?Instance:rdf_term, ?Class:iri, ?Graph:atom) is nondet.

rdf_instance(I, C):-
  rdf_instance(I, C, _).


%! rdf_instance(?Instance:rdf_term, ?Class:iri, ?Graph:atom) is nondet.

rdf_instance(I, C, G):-
  user:rdf(I, rdf:type, C, G).



%! rdf_langstring(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   +LanguagePriorityList:list(atom),
%!   ?Value:pair(atom,list(atom))
%! ) is nondet.

rdf_langstring(S, P, LRanges, V):-
  rdf_langstring(S, P, LRanges, V, _).


%! rdf_langstring(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   +LanguagePriorityList:list(atom),
%!   ?Value:pair(atom),
%!   ?Graph:atom
%! ) is nondet.

rdf_langstring(S, P, LRanges, V, G):-
  rdf_literal(S, P, rdf:langString, V, G),
  V = _-LTag,
  atom(LTag),
  basic_filtering(LRanges, LTag).



%! rdf_literal(?Subject:rdf_term, ?Predicate:iri, ?Value) is nondet.
% Wrapper around rdf_literal/4 where the datatype is uninstantiated.

rdf_literal(S, P, V):-
  rdf_literal(S, P, _, V, _).


%! rdf_literal(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value
%! ) is nondet.
% Wrapper around rdf_literal/4 where the graph is uninstantiated.

rdf_literal(S, P, D, V):-
  rdf_literal(S, P, D, V, _).


%! rdf_literal(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value,
%!   ?Graph:atom
%! ) is nondet.
% @see See rdf_literal_pl/[3-5] that allows native Prolog terms
%      to be returned in more situations, at the cost of correctness.

rdf_literal(S, P, D, V, G):-
  rdf_literal(S, P, D, V, G, _).


%! rdf_literal(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value,
%!   ?Graph:graph,
%!   -Triple:compound
%! ) is nondet.

% Language-tagged strings.
rdf_literal(S, P, D, Val, G, rdf(S,P,O,G)):-
  rdf_equal(D, rdf:langString),
  Val = Lex-LTag,
  O = literal(lang(LTag,Lex)),
  user:rdf(S, P, O, G),
  atom(LTag).
% Ground datatype and value.
rdf_literal(S, P, D, Val, G, rdf(S,P,O,G)):-
  ground(D),
  \+ rdf_equal(D, rdf:langString),
  ground(Val), !,
  % Map to lexical form.
  rdf_canonical_map(D, Val, literal(type(D,Lex))),
  (   rdf_equal(D, xsd:string),
      O = literal(Lex)
  ;   O = literal(type(D,Lex))
  ),
  user:rdf(S, P, O, G).
% Typed literal (as per RDF 1.0 specification).
rdf_literal(S, P, D, Val, G, rdf(S,P,Lit,G)):-
  (ground(D) -> \+ rdf_equal(D, rdf:langString) ; true),
  Lit = literal(type(D,_)),
  user:rdf(S, P, Lit, G),
  rdf_lexical_map(Lit, Val).
% Simple literal (as per RDF 1.0 specification).
rdf_literal(S, P, xsd:string, Val, G, rdf(S,P,O,G)):-
  O = literal(Lex),
  user:rdf(S, P, O, G),
  atom(Lex),
  rdf_global_id(xsd:string, D),
  rdf_lexical_map(literal(type(D,Lex)), Val).



%! rdf_literal_pl(?Subject:rdf_term, ?Predicate:iri, ?Value) is nondet.
% Wrapper around rdf_literal_pl/4 where the datatype in uninstantiated.

rdf_literal_pl(S, P, V):-
  rdf_literal_pl(S, P, _, V, _).


%! rdf_literal_pl(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value
%! ) is nondet.
% Wrapper around rdf_literal_pl/5 where the graph in uninstantiated.

rdf_literal_pl(S, P, D, V):-
  rdf_literal_pl(S, P, D, V, _).


%! rdf_literal_pl(
%!   ?Subject:rdf_term,
%!   ?Predicate:iri,
%!   ?Datatype:iri,
%!   ?Value,
%!   ?Graph:atom
%! ) is nondet.
% rdf_literal/[3-5] seeks to interpret the lexical form of an RDF datatype
% according to an RDF datatype.
%
% Sometimes this interpretation cannot be represented in a native Prolog term.
% It may then, for certain use cases, still be useful to return
% the native Prolog term which most closely matches the correct interpretation.
%
% For example, in XSD's dateTime/7 seconds are represented by a decimal,
% but in Prolog's date/9 and time/6 seconds are represented by a float.
% The latter comes quite close to the former, but will generally
% be a tiny bit less precise.
%
% In short: for correctness use rdf_literal/[3-5];
% for convencience at the cost of a little bit of correctness
% use rdf_literal_pl/[3-5].

rdf_literal_pl(S, P, D, V, G):-
  rdf_literal(S, P, D, V0, G),
  (xsd_datatype(D, date) -> xsd_date_to_prolog_term(V0, V) ; V = V0).
