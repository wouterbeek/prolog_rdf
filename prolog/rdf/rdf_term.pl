:- module(
  rdf_term,
  [
    rdf_bnode/1,	% ?BN
    rdf_bnode/2,	% +G, ?BN
    rdf_datatype_iri/1,	% ?D
    rdf_datatype_iri/2,	% +G, ?D
    rdf_iri/1,		% ?T
    rdf_iri/2,		% +G, ?Iri
    rdf_is_iri/1,	% @Term
    rdf_is_name/1,	% @Term
    rdf_is_term/1,	% @Term
    rdf_literal/1,	% ?Lit
    rdf_literal/2,	% +G, ?Lit
    rdf_name/1,		% ?Name:rdf_name
    rdf_name/2,		% +G, ?Name:rdf_name
    rdf_node/1,		% ?Node:rdf_term
    rdf_node/2,		% +G, ?Node:rdf_term
    rdf_object/1,	% ?O
    rdf_object/2,	% +G, ?O
    rdf_predicate/1,	% ?P
    rdf_predicate/2,	% +G, ?P
    rdf_subject/1,	% ?S
    rdf_subject/2,	% +G, ?S
    rdf_term/1,		% ?T
    rdf_term/2		% +G, ?T
  ]
).
:- reexport(library(semweb/rdf_db), [
     rdf_is_bnode/1,	% @Term
     rdf_is_literal/1	% @Term
   ]).

/** <module> Generalized RDF terms

Support for RDF 1.1 terms.

### rdf_is_resource/1

Semweb's rdf_is_resource/1 is quite misleading.
A first mistake one may make is to think that this predicate is about
semantics (resources being objects) while it actually is about syntax
(RDF terms that are either IRIs or blank nodes).
A second mistake one may make is to assume that rdf_is_resource/1 will
succeed for precisely those syntactic constructs that have a resource as
their interpretation.
But this is not the case either, since typed literals are mapped onto
resources as well.

---

@author Wouter Beek
@compat RDF 1.1 Concepts and Abstract Syntax
@see http://www.w3.org/TR/2014/REC-rdf11-concepts-20140225/
@version 2015/07-2015/08, 2015/10, 2015/12-2016/01
*/

:- use_module(library(rdf/id_store)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_literal)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(rdf11/rdf11), [
     rdf_bnode/1 as rdf_bnode_id,
     rdf_iri/1 as rdf_iri_id,
     rdf_literal/1 as rdf_literal_id,
     rdf_name/1 as rdf_name_id,
     rdf_node/1 as rdf_node_id,
     rdf_object/1 as rdf_object_id,
     rdf_predicate/1 as rdf_predicate_id,
     rdf_subject/1 as rdf_subject_id,
     rdf_term/1 as rdf_term_id
   ]).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).

:- rdf_meta(rdf_node(o)).
:- rdf_meta(rdf_node(r,o)).
:- rdf_meta(rdf_subject(o)).
:- rdf_meta(rdf_subject(r,o)).
:- rdf_meta(rdf_datatype_iri(r)).
:- rdf_meta(rdf_datatype_iri(r,r)).
:- rdf_meta(rdf_iri(r)).
:- rdf_meta(rdf_iri(r,r)).
:- rdf_meta(rdf_literal(o)).
:- rdf_meta(rdf_literal(r,o)).
:- rdf_meta(rdf_name(o)).
:- rdf_meta(rdf_name(r,o)).
:- rdf_meta(rdf_object(o)).
:- rdf_meta(rdf_object(r,o)).
:- rdf_meta(rdf_predicate(r)).
:- rdf_meta(rdf_predicate(r,r)).
:- rdf_meta(rdf_term(o)).
:- rdf_meta(rdf_term(r,o)).

:- multifile(error:has_type/2).
error:has_type(rdf_bnode, B) :-
  rdf_is_bnode(B).
error:has_type(rdf_graph, G) :-
  (   G == default
  ;   error:has_type(iri, G)
  ).
error:has_type(rdf_literal, Lit) :-
  rdf_is_literal(Lit).
error:has_type(rdf_name, N) :-
  (   error:has_type(iri, N)
  ;   error:has_type(rdf_literal, N)
  ).
error:has_type(rdf_statement, Stmt) :-
  (   error:has_type(rdf_triple, Stmt)
  ;   error:has_type(rdf_quadruple, Stmt)
  ).
error:has_type(rdf_quadruple, T) :-
  T = rdf(S,P,O,G),
  error:has_type(rdf_term, S),
  error:has_type(iri, P),
  error:has_type(rdf_term, O),
  error:has_type(iri, G).
error:has_type(rdf_term, T) :-
  (   error:has_type(rdf_bnode, T)
  ;   error:has_type(rdf_literal, T)
  ;   error:has_type(iri, T)
  ).
error:has_type(rdf_triple, T) :-
  T = rdf(S,P,O),
  error:has_type(rdf_term, S),
  error:has_type(iri, P),
  error:has_type(rdf_term, O).





%! rdf_bnode(+BN) is semidet.
%! rdf_bnode(-BN) is nondet.
% Enumerates the current blank node terms.
% Ensures that no blank node occurs twice.

rdf_bnode(BN) :-
  (   var(BN)
  ->  rdf_bnode_id(BNid),
      id_to_term(BNid, BN)
  ;   term_to_id(BN, BNid),
      rdf_bnode_id(BNid)
  ).


%! rdf_bnode(+G, +BN) is semidet.
%! rdf_bnode(+G, -BN) is nondet.

rdf_bnode(G, BN) :-
  rdf_bnode(BN),
  once(rdf_node(G, BN)).



%! rdf_datatype_iri(+D) is semidet.
%! rdf_datatype_iri(-D) is nondet.

rdf_datatype_iri(D) :-
  distinct(D, (rdf_literal(Lit), rdf_literal_data(datatype, Lit, D))).


%! rdf_datatype_iri(+G, +D) is semidet.
%! rdf_datatype_iri(+G, -D) is nondet.

rdf_datatype_iri(G, D) :-
  distinct(D, (rdf_literal(G, Lit), rdf_literal_data(datatype, Lit, D))).



%! rdf_iri(@Term) is semidet.
%! rdf_iri(-Term) is nondet.

rdf_iri(Iri) :-
  (   var(Iri)
  ->  rdf_iri_id(Iriid),
      id_to_term(Iriid, Iri)
  ;   term_to_id(Iri, Iriid),
      rdf_iri_id(Iriid)
  ).


%! rdf_iri(+G, +Iri) is semidet.
%! rdf_iri(+G, -Iri) is nondet.

rdf_iri(G, Iri) :-
  rdf_iri(Iri),
  once(rdf_term(G, Iri)).



%! rdf_is_iri(@Term) is semidet.

rdf_is_iri(T) :- is_iri(T).



%! rdf_is_name(@Term) is semidet.

rdf_is_name(N) :- rdf_is_iri(N), !.
rdf_is_name(N) :- rdf_is_literal(N).



%! rdf_is_term(@Term) is semidet.

rdf_is_term(N) :- rdf_is_name(N), !.
rdf_is_term(N) :- rdf_is_bnode(N).



%! rdf_literal(@Term) is semidet.
%! rdf_literal(-Lit) is nondet.

rdf_literal(Lit) :-
  (   var(Lit)
  ->  rdf_literal_id(Litid),
      id_to_term(Litid, Lit)
  ;   term_to_id(Lit, Litid),
      rdf_literal_id(Litid)
  ).


%! rdf_literal(+G, +Lit) is semidet.
%! rdf_literal(+G, -Lit) is nondet.

rdf_literal(G, Lit) :-
  rdf_literal(Lit),
  once(rdf_node(G, Lit)).



%! rdf_name(+Name:rdf_name) is semidet.
%! rdf_name(-Name:rdf_name) is nondet.

rdf_name(Name) :-
  rdf_term(Name),
  \+ rdf_is_bnode(Name).


%! rdf_name(+G, +Name:rdf_name) is semidet.
%! rdf_name(+G, -Name:rdf_name) is nondet.

rdf_name(G, Name) :-
  rdf_term(G, Name),
  \+ rdf_is_bnode(Name).



%! rdf_node(+Node:rdf_node) is semidet.
%! rdf_node(-Node:rdf_node) is nondet.

rdf_node(S) :-
  rdf_subject(S).
rdf_node(O) :-
  rdf_object(O),
  % Make sure there are no duplicates.
  \+ rdf_subject(O).


%! rdf_node(+G, +Node:rdf_node) is semidet.
%! rdf_node(+G, -Node:rdf_node) is nondet.

rdf_node(G, S) :-
  rdf_subject(G, S).
rdf_node(G, O) :-
  rdf_object(G, O),
  % Make sure there are no duplicates.
  \+ rdf_subject(G, O).



%! rdf_object(+O) is semidet.
%! rdf_object(-O) is nondet.

rdf_object(O) :-
  (   var(O)
  ->  rdf_object_id(Oid),
      id_to_term(Oid, O)
  ;   term_to_id(O, Oid),
      rdf_object_id(Oid)
  ).
  

%! rdf_object(+G, +O) is semidet.
%! rdf_object(+G, -O) is nondet.

rdf_object(G, O) :-
  graph_term_to_id(G, Gid),
  (   var(O)
  ->  rdf_object_id(Oid),
      once(rdf_id(_, _, Oid, Gid)),
      id_to_term(Oid, O)
  ;   term_to_id(O, Oid),
      once(rdf_id(_, _, Oid, Gid))
  ).



%! rdf_predicate(+P) is semidet.
%! rdf_predicate(-P) is nondet.

rdf_predicate(P) :-
  (   var(P)
  ->  rdf_predicate_id(Pid),
      id_to_term(Pid, P)
  ;   term_to_id(P, Pid),
      rdf_predicate_id(Pid)
  ).


%! rdf_predicate(+G, +P) is semidet.
%! rdf_predicate(+G, -P) is nondet.

rdf_predicate(G, P) :-
  graph_term_to_id(G, Gid),
  (   var(P)
  ->  rdf_predicate_id(Pid),
      once(rdf_id(_, Pid, _, Gid)),
      id_to_term(Pid, P)
  ;   term_to_id(P, Pid),
      once(rdf_id(_, Pid, _, Gid))
  ).



%! rdf_subject(+S) is semidet.
%! rdf_subject(-S) is nondet.

rdf_subject(S) :-
  (   var(S)
  ->  rdf_subject_id(Sid),
      id_to_term(Sid, S)
  ;   term_to_id(S, Sid),
      rdf_subject_id(Sid)
  ).


%! rdf_subject(+G, +S) is semidet.
%! rdf_subject(+G, -S) is nondet.

rdf_subject(G, S) :-
  graph_term_to_id(G, Gid),
  (   var(S)
  ->  rdf_subject_id(Sid),
      once(rdf_id(Sid, _, _, Gid)),
      id_to_term(Sid, S)
  ;   term_to_id(S, Sid),
      once(rdf_id(Sid, _, _, Gid))
  ).



%! rdf_term(@Term) is semidet.
%! rdf_term(-Term:rdf_term) is nondet.

rdf_term(P) :-
  rdf_predicate(P).
rdf_term(N) :-
  rdf_node(N),
  % Ensure there are no duplicates.
  \+ rdf_predicate(N).


%! rdf_term(+G, +T) is semidet.
%! rdf_term(+G, -T) is nondet.

rdf_term(G, P) :-
  rdf_predicate(G, P).
rdf_term(G, N) :-
  rdf_node(G, N),
  % Ensure there are no duplicates.
  \+ rdf_predicate(N).
