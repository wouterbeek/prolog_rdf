:- module(
  rdf_build,
  [
    rdf_assert/1,		% +Stmt
    rdf_assert/3,		% +S, +P, +O
    rdf_assert/4,		% +S, +P, +O, ?G
    rdf_assert_instance/2,	% +I, ?C
    rdf_assert_instance/3,	% +I, ?C, ?G
    rdf_assert_now/2,		% +S, +P
    rdf_assert_now/3,		% +S, +P, ?G
    rdf_assert_now/4,		% +S, +P, +D, ?G
    rdf_assert_property/3,	% +P, ?Q, ?G
    rdf_create_iri/2,		% +Prefix:atom, -Iri:atom
    rdf_create_iri/3,		% +Prefix:atom, +SubPaths:list(atom), -Iri:atom
    rdf_retractall/1,		% +Stmt
    rdf_retractall/3,		% ?S, ?P, ?O
    rdf_retractall/4,		% ?S, ?P, ?O, ?G
    rdf_retractall_term/1,	% +T
    rdf_retractall_term/2	% +T, ?G
  ]
).
:- reexport(library(rdf11/rdf11), [
     rdf_create_bnode/1,			% -B
     rdf_assert/3 as rdf_assert_id,		% +Sid, +Pid, +Oid
     rdf_assert/4 as rdf_assert_id,		% +Sid, +Pid, +Oid, +Gid
     rdf_retractall/3 as rdf_retractall_id,	% ?Sid, ?Pid, ?Oid
     rdf_retractall/4 as rdf_retractall_id,	% ?Sid, ?Pid, ?Oid, ?Gid
     op(100, xfx, @),
     op(650, xfx, ^^)
   ]).

/** <module> Generalized RDF building

Simple asserion and retraction predicates for RDF.

@author Wouter Beek
@compat RDF 1.1
@license MIT License
@version 2015/07-2015/10, 2015/12-2016/01
*/

:- use_module(library(datetime/datetime)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(rdf/id_store)).
:- use_module(library(rdf/rdf_datatype)).
:- use_module(library(rdf/rdf_default)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_read)).%
:- use_module(library(rdf/rdf_term)).
:- use_module(library(typecheck)).
:- use_module(library(uuid_ext)).
:- use_module(library(xsd/datetime/xsd_datetime_functions)).

:- rdf_meta
	rdf_assert(t),
	rdf_assert(o, r, o),
	rdf_assert(o, r, o, r),
	rdf_assert_instance(o, t),
	rdf_assert_instance(o, t, r),
	rdf_assert_now(o, r),
	rdf_assert_now(o, r, r),
	rdf_assert_now(o, r, r, r),
	rdf_assert_property(o, r, r),
	rdf_retractall(t),
	rdf_retractall(o, +, o),
	rdf_retractall(o, +, o, r),
	rdf_retractall_term(o),
	rdf_retractall_term(o, r).





%! rdf_assert(+Stmt) is det.
% Wrapper around rdf_assert/[3,4].
% Statement is of the form `rdf/[3,4]`.

rdf_assert(rdf(S,P,O)) :- !,
  rdf_assert(S, P, O).
rdf_assert(rdf(S,P,O,G)) :-
  rdf_assert(S, P, O, G).


%! rdf_assert(+S, +P, +O) is det.
% Wrapper around rdf_assert/4 that asserts in the default graph.

rdf_assert(S, P, O) :-
  rdf_assert(S, P, O, default).


%! rdf_assert(+S, +P, +O, ?G) is det.
% Allows literals to appear in the subject position
% and closes under identity.

% 1. Identity statements.
rdf_assert(S, P, O, _) :-
  % @tbd RDF prefix expansion breaks at seemingly random places.
  rdf_equal(owl:sameAs, P0),
  rdf_is_id(P0, P), !,
  store_id(S, O).
% 2. Statements other than the identity statement.
rdf_assert(S, P, O, G) :-
  maplist(assign_id, [S,P,O], [Sid,Pid,Oid]),
  defval(default, G),
  assign_graph_id(G, Gid),
  rdf_assert_id(Sid, Pid, Oid, Gid).



%! rdf_create_iri(+Prefix:atom, -Iri:atom) is det.
% Wrapper around rdf_create_iri/3 using no subpaths.

rdf_create_iri(Prefix, Iri) :-
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

rdf_create_iri(Prefix, SubPaths0, Iri) :-
  uuid_no_hyphen(Id),
  append(SubPaths0, [Id], SubPaths),
  atomic_list_concat(SubPaths, /, LocalName),
  % Resolve the absolute IRI against the base IRI denoted by the RDF prefix.
  rdf_global_id(Prefix:LocalName, Iri).



%! rdf_assert_instance(+I, ?C) is det.
% Wrapper around rdf_assert_instance/3 with uninstantiated graph.

rdf_assert_instance(I, C) :-
  rdf_assert_instance(I, C, _).


%! rdf_assert_instance(+I, ?C, ?G) is det.
% Asserts an instance/class relationship.
%
% The following triples are added to the database:
%
% ```nquads
% 〈TERM, rdf:type, CLASS, GRAPH〉
% ```

rdf_assert_instance(I, C, G) :-
  var(C), !,
  rdf_assert(I, rdf:type, rdfs:'Resource', G).
rdf_assert_instance(I, Cs, G) :-
  is_list(Cs), !,
  forall(member(C, Cs), rdf_assert(I, rdf:type, C, G)).
rdf_assert_instance(I, C, G) :-
  rdf_assert_instance(I, [C], G).



%! rdf_assert_now(+S, +P) is det.
% Wrapper around rdf_assert_now/3 with uninstantiated graph.

rdf_assert_now(S, P) :-
  rdf_assert_now(S, P, _).


%! rdf_assert_now(+S, +P, ?G) is det.
% Wrapper around rdf_assert_now/4 with datatype `xsd:dateTime`.

rdf_assert_now(S, P, G) :-
  rdf_assert_now(S, P, xsd:dateTime, G).


%! rdf_assert_now(+S, +P, +D, ?G) is det.
% Asserts the triple `〈S, P, 〈Datatype,Lex〉〉@Graph`
% where `Lex` denotes the date-time point of assertion
% under the given Datatype.

rdf_assert_now(S, P, D, G) :-
  get_datetime(Now),
  rdf_assert(S, P, Now^^D, G).



%! rdf_assert_property(+P, ?Q, ?G) is det.
% Asserts an RDF Property that belongs to a class of properties called Parent.
%
% Parent defaults to `rdf:Property`.

rdf_assert_property(P, Q, G) :-
  rdf_defval(rdf:'Property', Q),
  rdf_assert_instance(P, Q, G).



%! rdf_retractall(+Stmt) is det.

rdf_retractall(rdf(S,P,O)) :- !,
  rdf_retractall(S, P, O).
rdf_retractall(rdf(S,P,O,G)) :-
  rdf_retractall(S, P, O, G).


%! rdf_retractall(?S, ?P, ?O) is det.

rdf_retractall(S, P, O) :-
  rdf_retractall(S, P, O, _).


%! rdf_retractall(?S, ?P, ?O, ?G) is det.

rdf_retractall(S, P, O, G) :-
  maplist(var_or_term_to_id, [S,P,O], [Sid,Pid,Oid]),
  var_or_graph_term_to_id(G, Gid),
  rdf_retractall_id(Sid, Pid, Oid, Gid),
  maplist(remove_id, [Sid,Pid,Oid]).
var_or_term_to_id(T, Tid) :- (var(T) -> Tid = T ; term_to_id(T, Tid)).
var_or_graph_term_to_id(G, Gid) :- (var(G) -> Gid = G ; graph_term_to_id(G, Gid)).



%! rdf_retractall_term(+T) is det.
% Wrapper around rdf_retractall_term/2 with uninstantiated graph.

rdf_retractall_term(T) :-
  rdf_retractall_term(T, _).


%! rdf_retractall_term(+T, ?G) is det.
% Removes all triples in which the given RDF term occurs.

rdf_retractall_term(T, G) :-
  rdf_retractall(T, _, _, G),
  rdf_retractall(_, T, _, G),
  rdf_retractall(_, _, T, G).
