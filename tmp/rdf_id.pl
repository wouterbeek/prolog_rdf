:- module(
  rdf_id,
  [
    rdf_assert_id/3,     % +S, +P, +O
    rdf_assert_id/4,     % +S, +P, +O, ?G
    rdf_id/3,            % ?S, ?P, ?O
    rdf_id/4,            % ?S, ?P, ?O, ?G
    rdf_id/6,            % ?S, ?P, ?O, -Sid, -Pid, -Oid
    rdf_id/8,            % ?S, ?P, ?O, +G, -Sid, -Pid, -Oid, -Gid
    rdf_has_id/3,        % ?S, ?P, ?O
    rdf_reachable_id/3,  % ?S, ?P, ?O
    rdf_retractall_id/3, % ?S, ?P, ?O
    rdf_retractall_id/4, % ?S, ?P, ?O, ?G
    z_print_graph_id//1, % ?Gid
    z_print_graph_id//2, % ?Gid, +Opts
    z_print_id//1,       % +Tid
    z_print_id//2        % +Tid, +Opts
  ]
).
:- reexport(library(semweb/rdf11), [
     op(100, xfx, @),
     op(650, xfx, ^^)
   ]).
:- use_module(library(semweb/rdf_db), [
     rdf/3 as rdf0,                       % ?Sid, ?Pid, Oid
     rdf/4 as rdf0,                       % ?Sid, ?Pid, ?Oid, ?Gid
     rdf_assert/3 as rdf_assert0,         % +Sid, +Pid, +Oid
     rdf_assert/4 as rdf_assert0,         % +Sid, +Pid, +Oid, +Gid
     rdf_has/3 as rdf_has0,               % ?Sid, ?Pid, ?Oid
     rdf_reachable/3 as rdf_reachable0,   % ?Sid, ?Pid, ?Oid
     rdf_retractall/3 as rdf_retractall0, % ?Sid, ?Pid, ?Oid
     rdf_retractall/4 as rdf_retractall0  % ?Sid, ?Pid, ?Oid, ?Gid
   ]).

/** <module> Identity-closed RDF

@author Wouter Beek
@compat RDF 1.1 Concepts and Abstract Syntax
@license MIT License
@see http://www.w3.org/TR/rdf11-concepts/
@version 2015/07-2016/02, 2016/06
*/

:- use_module(library(date_time/date_time)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(list_ext)).
:- use_module(library(ltag/ltag_match)).
:- use_module(library(q/q_term)).
:- use_module(library(yall)).
:- use_module(library(z/z_print)).
:- use_module(library(z/z_term)).

:- rdf_meta
   rdf_assert_id(o, r, o),
   rdf_assert_id(o, r, o, r),
   rdf_has_id(o, r, o),
   rdf_id(o, r, o),
   rdf_id(o, r, o, r),
   rdf_id(o, r, o, -, -, -),
   rdf_id(o, r, o, r, -, -, -, -),
   rdf_reachable_id(o, r, o),
   rdf_retractall_id(o, +, o),
   rdf_retractall_id(o, +, o, r).



%! rdf_assert_id(+S, +P, +O) is det.
%! rdf_assert_id(+S, +P, +O, ?G) is det.
% Allows literals to appear in the subject position
% and closes under identity.

rdf_assert_id(S, P, O) :-
  rdf_assert_id(S, P, O, default).

% 1. Identity statements.
rdf_assert_id(S, P, O, G) :-
  % @tbd RDF prefix expansion breaks at seemingly random places.
  (   rdf_equal(owl:sameAs, P0),
      rdf_is_id(P0, P)
  ->  store_id(S, O)
  ;   % 2. Statements other than the identity statement.
      maplist(assign_id, [S,P,O], [Sid,Pid,Oid]),
      defval(default, G),
      assign_graph_id(G, Gid),
      rdf_assert0(Sid, Pid, Oid, Gid)
  ).



%! rdf_id(?S, ?P, ?O) is nondet.
%! rdf_id(?S, ?P, ?O, ?G) is nondet.
%! rdf_id(?S, ?P, ?O, -Sid, -Pid, -Oid) is nondet.
%! rdf_id(?S, ?P, ?O, ?G, -Sid, -Pid, -Oid, -Gid) is nondet.

rdf_id(S, P, O) :-
  rdf_id(S, P, O, _).

% 1. Identity statement.
rdf_id(S, P, O, G) :-
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
rdf_id(S, P, O, G) :-
  rdf_id(S, P, O, G, _, _, _, _).

rdf_id(S, P, O, Sid, Pid, Oid) :-
  rdf_id(S, P, O, _, Sid, Pid, Oid, _).

rdf_id(S, P, O, G, Sid, Pid, Oid, Gid) :-
  rdf11:pre_object(O, O0),
  maplist(matching_term, [S,P,O0], [Sid,Pid,Oid]),
  matching_graph_term(G, Gid),
  rdf0(Sid, Pid, Oid, Gid),
  maplist(id_to_term, [Sid,Pid,Oid], [S,P,O0]),
  graph_id_to_term(Gid, G),
  rdf11:post_object(O, O0).



%! rdf_has_id(?S, ?P, ?O) is nondet.

rdf_has_id(S, P, O) :-
  rdf_equal(owl:sameAs, P),
  (   ground(S)
  ->  term_to_term(S, O)
  ;   ground(O)
  ->  term_to_term(O, S)
  ;   % Enumerate identical terms in general.
      id_terms(Ts),
      member(S, O, Ts)
  ).
rdf_has_id(S, P, O) :-
  maplist(matching_term, [S,P,O], [Sid,Pid,Oid]),
  rdf_has0(Sid, Pid, Oid),
  maplist(id_to_term, [Sid,Pid,Oid], [S,P,O]).



%! rdf_reachable_id(?S, ?P, ?O) is nondet.

rdf_reachable_id(S, P, O) :-
  maplist(matching_term, [S,P,O], [Sid,Pid,Oid]),
  rdf_reachable0(Sid, Pid, Oid),
  maplist(id_to_term, [Sid,Pid,Oid], [S,P,O]).


%! rdf_retractall(+Stmt) is det.

rdf_retractall(rdf(S,P,O)) :- !,
  rdf_retractall(S, P, O).
rdf_retractall(rdf(S,P,O,G)) :-
  rdf_retractall(S, P, O, G).


%! rdf_retractall_id(?S, ?P, ?O) is det.
%! rdf_retractall_id(?S, ?P, ?O, ?G) is det.

rdf_retractall_id(S, P, O) :-
  rdf_retractall_id(S, P, O, _).

rdf_retractall_id(S, P, O, G) :-
  maplist(var_or_term_to_id, [S,P,O], [Sid,Pid,Oid]),
  var_or_graph_term_to_id(G, Gid),
  rdf_retractall0(Sid, Pid, Oid, Gid),
  maplist(remove_id, [Sid,Pid,Oid]).



%! z_print_graph_id(+Gid)// is det.
%! z_print_graph_id(+Gid, +Opts)// is det.

z_print_graph_id(Gid) -->
  z_print_graph_id(Gid, []).
z_print_graph_id(default, _) --> !,
  "default".
z_print_graph_id(Gid, Opts) -->
  z_print_id(Gid, Opts).



%! z_print_id(+Tid)// is det.
%! z_print_id(+Tid, +Opts)// is det.

z_print_id(Tid) -->
  z_print_id(Tid, []).
z_print_id(Tid, Opts) -->
  {id_to_terms(Tid, Ts)},
  set(z_print_term, Ts).



%! z_print_tuple_id(+Sid, +Pid, +Oid, ?Gid, +Opts) is det.

z_print_tuple_id(Sid, Pid, Oid, Gid, Opts) -->
  "〈",
  z_print_id(Sid, Opts),
  ", ",
  z_print_id(Pid, Opts),
  ", ",
  z_print_id(Oid, Opts),
  "〉",
  ({var(Gid)} -> "" ; z_print_graph_id(Gid, Opts)).





% HELPERS %

matching_term(T, Tid) :- (ground(T) -> term_to_id(T, Tid) ; true).
matching_graph_term(G, Gid) :- (ground(G) -> graph_term_to_id(G, Gid) ; true).
var_or_term_to_id(T, Tid) :- (var(T) -> Tid = T ; term_to_id(T, Tid)).
var_or_graph_term_to_id(G, Gid) :- (var(G) -> Gid = G ; graph_term_to_id(G, Gid)).
