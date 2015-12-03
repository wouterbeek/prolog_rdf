:- module(
  rdf_term,
  [
    rdf_bnode/1, % ?BlankNode
    rdf_bnode/2, % ?Graph:rdf_graph
                 % ?BlankNode:bnode
    rdf_datatype_iri/1, % ?Datatype
    rdf_datatype_iri/2, % ?Graph:rdf_graph
                        % ?Datatype:iri
    rdf_is_name/1, % @Term
    rdf_is_term/1, % @Term
    rdf_literal/1, % ?Literal
    rdf_literal/2, % ?Graph:rdf_graph
                   % ?Literal:rdf_literal
    rdf_name/1, % ?Name
    rdf_name/2, % ?Graph:rdf_graph
                % ?Name:rdf_name
    rdf_node/1, % ?Node
    rdf_node/2, % ?Graph:rdf_graph
                % ?Node:rdf_term
    rdf_object/1, % ?Object
    rdf_object/2, % ?Graph:rdf_graph
                  % ?Object:rdf_term
    rdf_predicate/1, % ?Predicate
    rdf_predicate/2, % ?Graph:rdf_graph
                     % ?Predicate:iri
    rdf_subject/1, % ?Subject
    rdf_subject/2, % ?Graph:rdf_graph
                   % ?Subject:rdf_term
    rdf_term/1, % ?Term
    rdf_term/2 % ?Graph:rdf_graph
               % ?Term:rdf_term
  ]
).

/** <module> Generalized RDF terms

Support for RDF 1.1 terms.

### rdf_is_resource/1

The predicate rdf_is_resource/1 in library(semweb/rdf_db) is quite misleading.
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
@version 2015/07-2015/08, 2015/10, 2015/12
*/

:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_literal)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(semweb/rdf_db), [
     rdf_subject/1 as rdf_subject_id,
     rdf_resource/1 as rdf_non_literal_node_id,
     rdf_current_predicate/1 as rdf_predicate_id,
     rdf_current_literal/1 as rdf_literal_part
   ]).
:- use_module(library(solution_sequences)).
:- use_module(library(typecheck)).

:- rdf_meta(rdf_node(o)).
:- rdf_meta(rdf_node(r,o)).
:- rdf_meta(rdf_subject(o)).
:- rdf_meta(rdf_subject(r,o)).
:- rdf_meta(rdf_datatype_iri(r)).
:- rdf_meta(rdf_datatype_iri(r,r)).
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
:- rdf_meta(rdf_is_name(o)).
:- rdf_meta(rdf_is_term(o)).





%! rdf_bnode(+BNode:bnode) is semidet.
%! rdf_bnode(-BNode:bnode) is nondet.
% Enumerates the current blank node terms.
% Ensures that no blank node occurs twice.
%
% @bug Unfortunate naming due to conflict with rdf_bnode/1 in rdf_db.pl.

rdf_bnode(B):-
  rdf_node(B),
  rdf_is_bnode(B).



%! rdf_bnode(+Graph:rdf_graph, +BNode:bnode) is semidet.
%! rdf_bnode(+Graph:rdf_graph, -BNode:bnode) is nondet.
%! rdf_bnode(-Graph:rdf_graph, +BNode:bnode) is nondet.
%! rdf_bnode(-Graph:rdf_graph, -BNode:bnode) is nondet.

rdf_bnode(G, B):-
  rdf_node(G, B),
  rdf_is_bnode(B).



%! rdf_datatype_iri(+Datatype:iri) is semidet.
%! rdf_datatype_iri(-Datatype:iri) is nondet.

rdf_datatype_iri(D):-
  distinct(D, (
    rdf_literal(Lit),
    rdf_literal_data(datatype, Lit, D)
  )).


%! rdf_datatype_iri(+Graph:rdf_graph, +Datatype:iri) is semidet.
%! rdf_datatype_iri(+Graph:rdf_graph, -Datatype:iri) is nondet.

rdf_datatype_iri(G, D):-
  distinct(D, (
    rdf_literal(G, Lit),
    rdf_literal_data(datatype, Lit, D)
  )).



%! rdf_iri(@Term) is semidet.
%! rdf_iri(-Term) is semidet.

rdf_iri(T):-
  nonvar(T), !,
  is_iri(T),
  term_to_id(T, Tid),
  (rdf_predicate_id(Tid), ! ; rdf_non_literal_node_id(Tid)).
rdf_iri(P):-
  rdf_predicate(P).
rdf_iri(N):-
  rdf_node(N),
  is_iri(N),
  % Avoid duplicates.
  \+ rdf_predicate(N).



%! rdf_literal(@Term) is semidet.
%! rdf_literal(-Literal:rdf_literal) is nondet.

rdf_literal(Lit):-
  var(Lit), !,
  rdf_literal_part(LitPart),
  Lit = literal(LitPart).
rdf_literal(Lit):-
  Lit = literal(LitPart),
  rdf_literal_part(LitPart).



%! rdf_literal(+Graph:rdf_graph, +Literal:compound) is semidet.
%! rdf_literal(+Graph:rdf_graph, -Literal:compound) is nondet.
%! rdf_literal(-Graph:rdf_graph, +Literal:compound) is nondet.
%! rdf_literal(-Graph:rdf_graph, -Literal:compound) is nondet.

% Literals that appear in some object position.
rdf_literal(G, Lit):-
  rdf_object(G, Lit),
  rdf_is_literal(Lit).
% Literals that only appear in the subject position.
rdf_literal(G, Lit):-
  



%! rdf_name(+Name:rdf_name) is semidet.
%! rdf_name(-Name:rdf_name) is nondet.

rdf_name(Name):-
  rdf_term(Name),
  \+ rdf_is_bnode(Name).


%! rdf_name(+Graph:rdf_graph, +Name:rdf_name) is semidet.
%! rdf_name(+Graph:rdf_graph, -Name:rdf_name) is nondet.
%! rdf_name(-Graph:rdf_graph, +Name:rdf_name) is nondet.
%! rdf_name(-Graph:rdf_graph, -Name:rdf_name) is nondet.

rdf_name(G, Name):-
  rdf_term(G, Name),
  \+ rdf_is_bnode(Name).



%! rdf_node(+Node:rdf_node) is semidet.
%! rdf_node(-Node:rdf_node) is nondet.
%
% @bug Naming conflict with Semweb's rdf_node/1.

rdf_node(S):-
  rdf_subject(S).
rdf_node(O):-
  rdf_object(O),
  % Make sure there are no duplicates.
  \+ rdf_subject(O).


%! rdf_node(+Graph:rdf_graph, +Node:rdf_node) is semidet.
%! rdf_node(+Graph:rdf_graph, -Node:rdf_node) is nondet.
%! rdf_node(-Graph:rdf_graph, +Node:rdf_node) is nondet.
%! rdf_node(-Graph:rdf_graph, -Node:rdf_node) is nondet.

rdf_node(G, S):-
  rdf_subject(G, S).
rdf_node(G, O):-
  rdf_object(G, O),
  % Make sure there are no duplicates.
  \+ rdf_subject(G, O).



%! rdf_object(+Object:rdf_term) is semidet.
%! rdf_object(-Object:rdf_term) is nondet.

rdf_object(O):-
  rdf_literal(O),
  % Excludes cases in which a literal only appears in deleted statements.
  rdf(_, _, O).
rdf_object(O):-
  var(O), !,
  rdf_non_literal_node(Oid),
  \+ rdf_is_literal(Oid),
  rdf(_, _, Oid),
  id_to_term(Oid, O).
rdf_object(O):-
  term_to_id(O, Oid),
  rdf_non_literal_node(Oid),
  \+ rdf_is_literal(Oid).
  

%! rdf_object(+Graph:rdf_graph, +Object:rdf_term) is semidet.
%! rdf_object(+Graph:rdf_graph, -Object:rdf_term) is nondet.
%! rdf_object(-Graph:rdf_graph, +Object:rdf_term) is nondet.
%! rdf_object(-Graph:rdf_graph, -Object:rdf_term) is nondet.

rdf_object(G, O):-
  var(O), !,
  rdf(_, _, O, G),
  rdf_object(O).
rdf_object(G, O):-
  rdf_object(O),
  % Also excludes cases in which a literal only appears in deleted statements.
  rdf(_, _, O, G).



%! rdf_predicate(+Predicate:iri) is semidet.
%! rdf_predicate(-Predicate:iri) is nondet.
% @bug Terminologically more consistent than Semweb's rdf_current_predicate/1.

rdf_predicate(P):-
  rdf_current_predicate(P).


%! rdf_predicate(+Graph:rdf_graph, +Predicate:iri) is semidet.
%! rdf_predicate(+Graph:rdf_graph, -Predicate:iri) is nondet.

rdf_predicate(G, P):-
  rdf_predicate(P),
  rdf(_, P, _, G).



%! rdf_subject(+Subject:rdf_term) is semidet.
%! rdf_subject(-Subject:rdf_term) is nondet.

rdf_subject(S):-
  nonvar(S), !,
  (   rdf_is_literal(S)
  ->  literal_id(S, Sid),
      rdf_subject(Sid)
  ;   term_to_id(S, Sid),
      rdf_subject(Sid)
  ).
rdf_subject(S):-
  % NONDET
  rdf_subject(Sid),
  (   id_to_literal(Sid, S), !
      % NONDET
  ;   id_to_term(Sid, S)
  ).


%! rdf_subject(+Graph:rdf_graph, +Subject:rdf_term) is semidet.
%! rdf_subject(+Graph:rdf_graph, -Subject:rdf_term) is nondet.
%! rdf_subject(-Graph:rdf_graph, +Subject:rdf_term) is nondet.
%! rdf_subject(-Graph:rdf_graph, -Subject:rdf_term) is nondet.

rdf_subject(G, S):-
  nonvar(S), !,
  rdf_subject(S),
  rdf(S, _, G).
rdf_subject(G, S):-
  rdf(S, _, G),
  rdf_subject(S).



%! rdf_term(@Term) is semidet.
%! rdf_term(-Term:rdf_term) is nondet.

rdf_term(P):-
  rdf_predicate(P).
rdf_term(N):-
  rdf_node(N),
  % Ensure there are no duplicates.
  \+ rdf_predicate(N).


%! rdf_term(+Graph:rdf_graph, +Term:rdf_term) is semidet.
%! rdf_term(+Graph:rdf_graph, -Term:rdf_term) is nondet.
%! rdf_term(-Graph:rdf_graph, +Term:rdf_term) is nondet.
%! rdf_term(-Graph:rdf_graph, -Term:rdf_term) is nondet.

rdf_term(G, P):-
  rdf_predicate(G, P).
rdf_term(G, N):-
  rdf_node(G, N),
  % Ensure there are no duplicates.
  \+ rdf_predicate(N).
