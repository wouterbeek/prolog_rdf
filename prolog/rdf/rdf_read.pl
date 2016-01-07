:- module(
  rdf_read,
  [
    rdf/1,              % ?Stmt
    rdf/2,              % ?Trip, ?G
    rdf/3,              % ?S, ?P, ?O
    rdf/4,              % ?S, ?P, ?O, ?G
    rdf/6,              % ?S, ?P, ?O, -Sid, -Pid, -Oid
    rdf/8,              % ?S, ?P, ?O, ?G, -Sid, -Pid, -Oid, -Gid
    rdf_date/3,         % ?S, ?P, ?Date
    rdf_date/4,         % ?S, ?P, ?Date, ?G
    rdf_has/3,          % ?S, ?P, ?O
    rdf_instance/2,     % ?I, ?C
    rdf_instance/3,     % ?I, ?C, ?G
    rdf_langstring/3,   % ?S, ?P, ?V
    rdf_langstring/4,   % ?S, ?P, +LanguagePriorityList, ?V
    rdf_langstring/5,   % ?S, ?P, +LanguagePriorityList, ?V, ?G
    rdf_literal/3,      % ?S, ?P, ?V
    rdf_literal/4,      % ?S, ?P, ?D, ?V
    rdf_literal/5,      % ?S, ?P, ?D, ?V, ?G
    rdf_one_string/3,   % +S, +P, -V:atom
    rdf_pref_string/3,  % ?S, ?P, -Lex
    rdf_pref_string/4,  % ?S, ?P, +LanguagePriorityList, -Lex
    rdf_pref_string/5,  % ?S, ?P, +LanguagePriorityList, -LTag, -Lex
    rdf_pref_string/6,  % ?S, ?P, +LanguagePriorityList, -LTag, -Lex, ?G
    rdf_reachable/3     % ?S, ?P, ?O
  ]
).
:- reexport(library(rdf11/rdf11), [
     op(100, xfx, @),
     op(650, xfx, ^^)
   ]).
:- reexport(library(semweb/rdf_db), [
     rdf/3 as rdf_id,                     % ?Sid, ?Pid, Oid
     rdf/4 as rdf_id,                     % ?Sid, ?Pid, ?Oid, ?Gid
     rdf_has/3 as rdf_has_id,             % ?Sid, ?Pid, ?Oid
     rdf_reachable/3 as rdf_reachable_id  % ?Sid, ?Pid, ?Oid
   ]).

/** <module> Generalized RDF reading

@author Wouter Beek
@compat RDF 1.1 Concepts and Abstract Syntax
@license MIT License
@see http://www.w3.org/TR/rdf11-concepts/
@version 2015/07-2016/01
*/

:- use_module(library(datetime/datetime)).
:- use_module(library(error)).
:- use_module(library(list_ext)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(rdf/id_store)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(xsd/xsd)).

:- rdf_meta
   rdf(t),
   rdf(t, r),
   rdf(o, r, o),
   rdf(o, r, o, r),
   rdf(o, r, o, -, -, -),
   rdf(o, r, o, r, -, -, -, -),
   rdf_date(o, r, ?),
   rdf_date(o, r, ?, r),
   rdf_has(o, r, o),
   rdf_instance(o, r),
   rdf_instance(o, r, r),
   rdf_langstring(o, r, ?),
   rdf_langstring(o, r, +, ?),
   rdf_langstring(o, r, +, ?, r),
   rdf_literal(o, r, ?),
   rdf_literal(o, r, r, ?),
   rdf_literal(o, r, r, ?, r),
   rdf_one_string(o, r, -),
   rdf_pref_string(o, r, -),
   rdf_pref_string(o, r, +, -),
   rdf_pref_string(o, r, +, -, -),
   rdf_pref_string(o, r, +, -, -, r),
   rdf_reachable(o, r, o).





%! rdf(?Stmt) is nondet.
% Wrapper around rdf/4.

rdf(rdf(S,P,O)) :-
  rdf(S, P, O, _).
rdf(rdf(S,P,O,G)) :-
  rdf(S, P, O, G).


%! rdf(+Trip, ?G) is nondet.

rdf(rdf(S,P,O), G) :-
  rdf(S, P, O, G).


%! rdf(?S, ?P, ?O) is nondet.
% Wrapper around rdf/4 reading from the default graph.

rdf(S, P, O) :-
  rdf(S, P, O, _).


%! rdf(?S, ?P, ?O, ?G) is nondet.

% 1. Identity statement.
rdf(S, P, O, G) :-
  rdf_equal(owl:sameAs, P),
  (   ground(S)
  ->  term_to_term(S, O)
  ;   ground(O)
  ->  term_to_term(O, S)
  ;   % Enumerate identical terms in general.
      var(G)
  ->  id_terms(Ts),
      member(S, O, Ts)
  ;   % Enumerate identical terms at least one of which
      % occurs in the given graph.
      rdf_term(G, S),
      term_to_term(S, O)
  ).
% 2. Statements other than identity statements.
rdf(S, P, O, G) :-
  rdf(S, P, O, G, _, _, _, _).


%! rdf(?S, ?P, ?O, -Sid, -Pid, -Oid) is nondet.

rdf(S, P, O, Sid, Pid, Oid) :-
  rdf(S, P, O, _, Sid, Pid, Oid, _).


%! rdf(?S, ?P, ?O, ?G, -Sid, -Pid, -Oid, -Gid) is nondet.

rdf(S, P, O, G, Sid, Pid, Oid, Gid) :-
  rdf11:pre_object(O, O0),
  maplist(matching_term, [S,P,O0], [Sid,Pid,Oid]),
  matching_graph_term(G, Gid),
  rdf_id(Sid, Pid, Oid, Gid),
  maplist(id_to_term, [Sid,Pid,Oid], [S,P,O0]),
  graph_id_to_term(Gid, G),
  rdf11:post_object(O, O0).



%! rdf_date(?S, ?P, ?Datetime:datetime) is nondet.
%! rdf_date(?S, ?P, ?Datetime:datetime, ?G) is nondet.
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

rdf_date(S, P, V) :-
  rdf_date(S, P, V, _).
rdf_date(S, P, V2, G) :-
  rdf_literal(S, P, xsd:date, V1, G),
  date_to_datetime(V1, V2).
rdf_date(S, P, V2, G) :-
  rdf_literal(S, P, xsd:dateTime, V1, G),
  date_to_datetime(V1, V2).
rdf_date(S, P, V2, G) :-
  rdf_literal(S, P, xsd:gDay, V1, G),
  date_to_datetime(V1, V2).
rdf_date(S, P, V2, G) :-
  rdf_literal(S, P, xsd:gMonth, V1, G),
  date_to_datetime(V1, V2).
rdf_date(S, P, V2, G) :-
  rdf_literal(S, P, xsd:gMonthDay, V1, G),
  date_to_datetime(V1, V2).
rdf_date(S, P, V2, G) :-
  rdf_literal(S, P, xsd:gYear, V1, G),
  date_to_datetime(V1, V2).
rdf_date(S, P, V2, G) :-
  rdf_literal(S, P, xsd:gYearMonth, V1, G),
  date_to_datetime(V1, V2).
rdf_date(S, P, V2, G) :-
  rdf_literal(S, P, xsd:time, V1, G),
  date_to_datetime(V1, V2).



%! rdf_has(?S, ?P, ?O) is nondet.

rdf_has(S, P, O) :-
  rdf_equal(owl:sameAs, P),
  (   ground(S)
  ->  term_to_term(S, O)
  ;   ground(O)
  ->  term_to_term(O, S)
  ;   % Enumerate identical terms in general.
      id_terms(Ts),
      member(S, O, Ts)
  ).
rdf_has(S, P, O) :-
  maplist(matching_term, [S,P,O], [Sid,Pid,Oid]),
  rdf_has_id(Sid, Pid, Oid),
  maplist(id_to_term, [Sid,Pid,Oid], [S,P,O]).



%! rdf_instance(?I, ?C) is nondet.
% Wrapper around rdf_instance/3 with uninstantiated graph.

rdf_instance(I, C) :-
  rdf_instance(I, C, _).


%! rdf_instance(?I, ?C, ?G) is nondet.

rdf_instance(I, C, G) :-
  rdf_equal(rdf:type, P),
  rdf(I, P, C, G).



%! rdf_langstring(?S, ?P, ?V:pair(atom)) is nondet.
% Wrapper around rdf_langstring/4 using the global language priority
% list setting.

rdf_langstring(S, P, V) :-
  user:setting(language_priority_list, LRanges),
  rdf_langstring(S, P, LRanges, V).


%! rdf_langstring(?S, ?P, +LanguagePriorityList:list(atom), ?V:pair(atom)) is nondet.
% Wrapper around rdf_langstring/5 with uninstantiated graph.

rdf_langstring(S, P, LRanges, V) :-
  rdf_langstring(S, P, LRanges, V, _).


%! rdf_langstring(?S, ?P, +LanguagePriorityList:list(atom), ?V:pair(atom), ?G) is nondet.
% Matches RDF statements whose object term is a language-tagged string
% that mathes the given language priory list.
% Notice that results for each of the prioritized languages are given
% in arbitrary order.

rdf_langstring(S, P, LRanges, Lex-LTag, G) :-
  rdf(S, P, Lex@LTag, G),
  atom(LTag),
  basic_filtering(LRanges, LTag).



%! rdf_literal(?S, ?P, ?V) is nondet.
% Wrapper around rdf_literal/4 with uninstantiated datatype.

rdf_literal(S, P, V) :-
  rdf_literal(S, P, _, V).


%! rdf_literal(?S, ?P, ?D, ?V) is nondet.
% Wrapper around rdf_literal/5 with uninstantiated graph.

rdf_literal(S, P, D, V) :-
  rdf_literal(S, P, D, V, _).


%! rdf_literal(?S, ?P, ?D, ?V, ?G) is nondet.
% This predicate is only used internally, by other predicates in rdf_read,
% since it makes available the RDF statement in Quadruple.

% Language-tagged strings.
rdf_literal(S, P, D, Lex-LTag, G) :-
  rdf_equal(rdf:langString, D),
  rdf(S, P, Lex@LTag, G).
% Other datatypes.
rdf_literal(S, P, D, V, G) :-
  rdf(S, P, V^^D, G).


%! rdf_one_string(+S, +P, -V:atom) is det.

rdf_one_string(S, P, X) :-
  rdf_pref_string(S, P, X), !.
rdf_one_string(_, _, '∅').



%! rdf_pref_string(?S, ?P, -Lex) is nondet.
% Wrapper around rdf_pref_string/4 using the global language priority
% list setting.

rdf_pref_string(S, P, Lex) :-
  user:setting(language_priority_list, LRanges),
  rdf_pref_string(S, P, LRanges, Lex).


%! rdf_pref_string(?S, ?P, +LanguagePriorityList:list(atom), -Lex) is nondet.
% Wrapper around rdf_pref_string/5 that does not return the language tag,
% if any.

rdf_pref_string(S, P, LRanges, Lex) :-
  rdf_pref_string(S, P, LRanges, _, Lex).


%! rdf_pref_string(?S, ?P, +LanguagePriorityList:list(atom), -LTag, -Lex) is nondet.
% Wrapper around rdf_pref_string/6 with uninstantiated graph.

rdf_pref_string(S, P, LRanges, LTag, Lex) :-
  rdf_pref_string(S, P, LRanges, LTag, Lex, _).


%! rdf_pref_string(?S, ?P, +LanguagePriorityList:list(atom), -LTag, -Lex, ?G) is nondet.
% Returns, in this exact order:
%   1. The language-tagged strings that match the given
%      language priority list; returning results for higher
%      priority language earlier.
%   2. The language-tagged strings that do not match the given
%      language priority list.
%   3. XSD strings.

% Language priority list matches.
rdf_pref_string(S, P, LRanges, LTag, Lex, G) :-
  member(LRange, LRanges),
  rdf_langstring(S, P, [LRange], Lex-LTag, G).
% Other language-tagged strings.
rdf_pref_string(S, P, LRanges, LTag, Lex, G) :-
  rdf_literal(S, P, rdf:langString, Lex-LTag, G),
  % Avoid duplicates.
  \+ basic_filtering(LRanges, LTag).
% Plain XSD strings.
rdf_pref_string(S, P, _, _, Lex, G) :-
  rdf_literal(S, P, xsd:string, Lex, G).



%! rdf_reachable(?S, ?P, ?O) is nondet.

rdf_reachable(S, P, O) :-
  maplist(matching_term, [S,P,O], [Sid,Pid,Oid]),
  rdf_reachable_id(Sid, Pid, Oid),
  maplist(id_to_term, [Sid,Pid,Oid], [S,P,O]).





% HELPERS %

matching_term(T, Tid) :- (ground(T) -> term_to_id(T, Tid) ; true).
matching_graph_term(G, Gid) :- (ground(G) -> graph_term_to_id(G, Gid) ; true).
